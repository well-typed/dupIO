{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE UnboxedTuples     #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -O0 -fno-full-laziness -fno-state-hack #-}

module Main (main) where

import Data.Dup (dup)
import GHC.Exts (Int(..), (-#), State#, RealWorld, Int#)
import GHC.IO (IO(..), unIO)
import Prelude (print)
import System.Environment (getArgs)

import qualified Prelude as P

{-------------------------------------------------------------------------------
  Mini prelude

  We define these here to get a better profile
-------------------------------------------------------------------------------}

type UnwrappedIO a = State# RealWorld -> (# State# RealWorld, a #)

{-# NOINLINE prev #-}
prev :: Int# -> Int#
prev i = i -# 1#

{-# NOINLINE twice #-}
twice :: IO () -> IO ()
twice (IO f) = IO (\w0 -> case f w0 of (# w1, _ #) -> f w1)

{-# NOINLINE printInt #-}
printInt :: Int# -> UnwrappedIO ()
printInt n = unIO (print (I# n))

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
      ["runSource0"] -> twice (runSource0 (yieldFrom 1_000_000#))
      ["runSource1"] -> twice (runSource1 (yieldFrom 1_000_000#))
      ["runSource2"] -> twice (runSource2 (yieldFrom 1_000_000#))
      _otherwise     -> P.putStrLn "Invalid arguments"
