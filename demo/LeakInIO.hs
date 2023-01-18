{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -O0 -fno-full-laziness -fno-state-hack #-}

module Main (main) where

import System.Environment
import GHC.IO

{-------------------------------------------------------------------------------
  Demonstration
-------------------------------------------------------------------------------}

-- | Version 1: leaks
version1 :: Int -> IO ()
version1 0 = return ()
version1 n = do print n ; version1 (n - 1)

-- | Version 2: runs in constant space
version2 :: Int -> IO ()
version2 0 = return ()
version2 n = do () <- return () ; print n ; version2 (n - 1)

{-------------------------------------------------------------------------------
  Explanation
-------------------------------------------------------------------------------}

-- | Leaking version
--
-- This illustrates what is happening in 'version1'. In the main application, we
-- have
--
-- > twice $ version3 1_000_000
--
-- This is a thunk that when executed will /first/ allocate the thunk for the
-- next recursive call, and /then/ proceed. Since we will execute it again
-- (because of @twice@), that "recursive thunk" cannot be GCed. This happens at
-- every step along the way, and hence we construct a whole chain of PAP
-- closures (all waiting the world argument).
version3 :: Int -> IO ()
version3 0 = IO $ \w -> (# w, () #)
version3 n = let p_r = print n >> version3 (n - 1)
             in IO $ \w -> unIO p_r w

-- | Non-leaking version
--
-- This illustrates what is happening in 'version2'. The difference here is that
-- the thunk for the next recursive call is allocated /inside/ the IO function;
-- this mean that it will happen every time the function is called, and then
-- immediately GCed.
--
-- In 'version2' this happens because the @() <- return ()@ forces the let
-- to happen after something has already happened, i.e., inside the body of
-- the function.
--
-- With optimizations, version3 would be turned into version4 due to the
-- state hack, illustration its importance. However, it is of course unfortunate
-- that we have to rely on such optimizations, especially since they can be
-- brittle, and sometimes not what we want. See also
-- <https://stackoverflow.com/questions/29404065/why-does-this-haskell-code-run-slower-with-o/30603291#30603291>.
version4 :: Int -> IO ()
version4 0 = IO $ \w -> (# w, () #)
version4 n = IO $ \w ->
               let p_r = print n >> version4 (n - 1)
               in unIO p_r w

{-------------------------------------------------------------------------------
  Application

  Suggested invocation:

  > cabal run demo-leak-in-io -- +RTS -s -RTS version1 >out

  or

  > cabal run demo-leak-in-io --enable-profiling -- +RTS -s -hy -RTS version1 >out
-------------------------------------------------------------------------------}

twice :: IO () -> IO ()
twice io = io >> io

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["version1"] -> twice $ version1 1_000_000
      ["version2"] -> twice $ version2 1_000_000
      ["version3"] -> twice $ version3 1_000_000
      ["version4"] -> twice $ version4 1_000_000
      _otherwise -> error "invalid arguments"