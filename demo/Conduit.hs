{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE UnboxedTuples     #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -O0 -fno-full-laziness -fno-state-hack #-}

module Main (main) where

import Prelude (print)
import Data.Dup.IO (dup)
import GHC.IO (IO(..), unIO)
import GHC.Exts (Int(..), (-#), State#, RealWorld, Int#)
import qualified Prelude as P
import System.Environment (getArgs)

{-------------------------------------------------------------------------------
  Mini prelude

  We define these here to get a better profile
-------------------------------------------------------------------------------}

type UnwrappedIO a = State# RealWorld -> (# State# RealWorld, a #)

{-# NOINLINE prev #-}
prev :: Int# -> Int#
prev i = i -# 1#

{-# NOINLINE thenIO #-}
thenIO :: UnwrappedIO () -> UnwrappedIO a -> UnwrappedIO a
thenIO f g = \w0 -> case f w0 of (# w1, _ #) -> g w1

{-# NOINLINE twice #-}
twice :: IO () -> IO ()
twice (IO f) = IO (\w0 -> case f w0 of (# w1, _ #) -> f w1)

{-# NOINLINE printInt #-}
printInt :: Int# -> UnwrappedIO ()
printInt n = unIO (print (I# n))

{-------------------------------------------------------------------------------
  Demonstration of allocation in IO

  > reference :: IO ()
  > reference = go 1_000_000#
  >   where
  >     go :: Int# -> IO ()
  >     go 0# = return ()
  >     go n  = print (I# n) >> go (n -# 1#)

  With very minor modifications, this code might either become 'badAlloc' (which
  leaks when executed twice) or 'goodAlloc' (which doesn't).

  Most of the time we depend on the ghc optimizer to do the right thing here,
  and give us the behaviour we want; in particular, it depends very heavily on
  the state hack.

  What this means for a reliable demonstration of 'dup' is that we should not
  rely on any of these abstractions, and write our code in a very low level way;
  we don't want memory leaks arising from IO actions being allocated. In
  particular, the use of 'Data.Dup.dupIO' instead of 'dup' can result in result
  in a change from a 'badAlloc'-like code to a 'goodAlloc'-like code, but in a
  way that is completely unrelated to 'dup'-vs-'Data.Dup.dupIO'.
-------------------------------------------------------------------------------}

badAlloc :: IO ()
badAlloc = IO (go 1_000_000#)
  where
    go :: Int# -> State# RealWorld -> (# State# RealWorld, () #)
    go n =
        case n of
          0# -> \w0 -> (# w0, () #)
          _  -> let p  = printInt n
                    r  = go (prev n)
                    pr = thenIO p r
                in pr

goodAlloc :: IO ()
goodAlloc = IO (go 1_000_000#)
  where
    go :: Int# -> State# RealWorld -> (# State# RealWorld, () #)
    go n =
        case n of
          0# -> \w0 -> (# w0, () #)
          _  -> \w0 -> let p  = printInt n
                           r  = go (prev n)
                           pr = thenIO p r
                in pr w0

{-------------------------------------------------------------------------------
  Source definition
-------------------------------------------------------------------------------}

data Source = Done | Yield Int# Source

yieldFrom :: Int# -> Source
yieldFrom 0# = Done
yieldFrom n  = let !n' = prev n
               in Yield n (yieldFrom n')

{-------------------------------------------------------------------------------
  Various interpreters
-------------------------------------------------------------------------------}

-- | First version: no 'dup'
--
-- This version leaks (when executed twice with the same conduit)
runSource0 :: Source -> IO ()
runSource0 = \src -> IO (go src)
  where
    go :: Source -> UnwrappedIO ()
    go src =
        case src of
          Done      -> \w0 -> (# w0, () #)
          Yield n k -> \w0 -> case printInt n w0 of
                                (# w1, _ #) -> go k w1

-- | Leak-free version: 'dup' before pattern matching
runSource1 :: Source -> IO ()
runSource1 = \src -> IO (go src)
  where
    go :: Source -> UnwrappedIO ()
    go src =
        case dup src of
          Done      -> \w0 -> (# w0, () #)
          Yield n k -> \w0 -> case printInt n w0 of
                                (# w1, _ #) -> go k w1

-- | Variation on 'runSource1', 'dup' /inside/ the IO action
runSource2 :: Source -> IO ()
runSource2 = \src -> IO (go src)
  where
    go :: Source -> UnwrappedIO ()
    go src = \w0 ->
        case dup src of
          Done      -> (# w0, () #)
          Yield n k -> case printInt n w0 of
                         (# w1, _ #) -> go k w1

{-------------------------------------------------------------------------------
  Application driver

  Example execution:

  > cabal run demo-conduit -- goodAlloc +RTS -s -l -hi -RTS >out
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["goodAlloc"]  -> twice goodAlloc
      ["badAlloc"]   -> twice badAlloc
      ["runSource0"] -> twice (runSource0 (yieldFrom 1_000_000#))
      ["runSource1"] -> twice (runSource1 (yieldFrom 1_000_000#))
      ["runSource2"] -> twice (runSource2 (yieldFrom 1_000_000#))
      _otherwise     -> P.putStrLn "Invalid arguments"
