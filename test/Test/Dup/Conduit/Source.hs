module Test.Dup.Conduit.Source (tests) where

import Prelude hiding (IO, (<*))

import Data.IORef
import Test.Tasty

import Data.Dup.IO (dup)

import Test.Util.Conduit.Source
import Test.Util.TestSetup

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Dup.Conduit.Source" [
      testGroup "Pure" [
         testLocalOOM "withoutDup.OOM" test_withoutDup
       , testCaseInfo "innerDup.OK"    test_innerDup
       ]
    , testGroup "IO" [
         testLocalOOM "execWithoutDup"       test_execWithoutDup
       , testCaseInfo "execWithDup_variant1" test_execWithDup_variant1
       , testCaseInfo "execWithDup_variant2" test_execWithDup_variant2
       , testCaseInfo "execWithDup_variant3" test_execWithDup_variant3
       , testCaseInfo "execWithDup_variant4" test_execWithDup_variant4
       ]
    ]

{-------------------------------------------------------------------------------
  Tests for the pure interpreter
-------------------------------------------------------------------------------}

test_withoutDup :: IO String
test_withoutDup = \w0 ->
    let c = yieldFrom limit

        runConduit :: IO Int
        runConduit = \w' -> let result = evalConduit c
                            in evaluate result w'

        !(# w1, _sum #) = retry (runConduit <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_innerDup :: IO String
test_innerDup = \w0 ->
    let c = yieldFrom limit

        runConduit :: IO Int
        runConduit = \w' -> let result = innerDup c
                            in evaluate result w'

        !(# w1, _sum #) = retry (runConduit <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

{-------------------------------------------------------------------------------
  Tests for the IO-based interpreter

  TODO: Something is wrong here. demo/Conduit.hs does NOT confirm this.
-------------------------------------------------------------------------------}

test_execWithoutDup :: IO String
test_execWithoutDup = \w0 ->
    let c = yieldFrom limit

        !(# w1, ref  #) = unwrapIO (newIORef 0) w0
        !(# w2, _sum #) = replicateM_ 5 (execWithoutDup ref c <* checkMem (1 * mb)) w1

    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_execWithDup_variant1 :: IO String
test_execWithDup_variant1 = \w0 ->
    let c = yieldFrom limit

        !(# w1, ref  #) = unwrapIO (newIORef 0) w0
        !(# w2, _sum #) = replicateM_ 5 (execWithDup_variant1 ref c <* checkMem (1 * mb)) w1

    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_execWithDup_variant2 :: IO String
test_execWithDup_variant2 = \w0 ->
    let c = yieldFrom limit

        !(# w1, ref  #) = unwrapIO (newIORef 0) w0
        !(# w2, _sum #) = replicateM_ 5 (execWithDup_variant2 ref c <* checkMem (1 * mb)) w1

    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_execWithDup_variant3 :: IO String
test_execWithDup_variant3 = \w0 ->
    let c = yieldFrom limit

        !(# w1, ref  #) = unwrapIO (newIORef 0) w0
        !(# w2, _sum #) = replicateM_ 5 (execWithDup_variant3 ref c <* checkMem (1 * mb)) w1

    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_execWithDup_variant4 :: IO String
test_execWithDup_variant4 = \w0 ->
    let c = yieldFrom limit

        !(# w1, ref  #) = unwrapIO (newIORef 0) w0
        !(# w2, _sum #) = replicateM_ 5 (execWithDup_variant4 ref c <* checkMem (1 * mb)) w1

    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

{-------------------------------------------------------------------------------
  Pure interpreter
-------------------------------------------------------------------------------}

{-# NOINLINE evalConduit #-}
evalConduit :: Source Int () -> Int
evalConduit = go 0
  where
    go :: Int -> Source Int () -> Int
    go acc (Done ())   = acc
    go acc (Yield b k) = let !acc' = acc + b in go acc' k

{-# NOINLINE innerDup #-}
innerDup :: Source Int () -> Int
innerDup = go 0
  where
    go :: Int -> Source Int () -> Int
    go acc c =
        case dup c of
          Done ()   -> acc
          Yield b k -> let !acc' = acc + b in go acc' k

{-------------------------------------------------------------------------------
  IO-based interpreter

  We test various variations, corresponding to ways in which ghc might move
  allocations around.
-------------------------------------------------------------------------------}

{-# NOINLINE execWithoutDup #-}
execWithoutDup :: IORef Int -> Source Int () -> IO ()
execWithoutDup ref = go
  where
    go :: Source Int () -> IO ()
    go (Done ())   = \w0 -> (# w0, () #)
    go (Yield b k) = \w0 -> let !(# w1, () #) = unwrapIO (modifyIORef' ref (+ b)) w0
                            in go k w1

{-# NOINLINE execWithDup_variant1 #-}
execWithDup_variant1 :: IORef Int -> Source Int () -> IO ()
execWithDup_variant1 ref = go
  where
    go :: Source Int () -> IO ()
    go c = \w0 ->
        let c' = dup c
        in go' c' w0

    go' :: Source Int () -> IO ()
    go' (Done ())   = \w0 -> (# w0, () #)
    go' (Yield b k) = \w0 -> let !(# w1, () #) = unwrapIO (modifyIORef' ref (+ b)) w0
                             in go k w1

{-# NOINLINE execWithDup_variant2 #-}
execWithDup_variant2 :: IORef Int -> Source Int () -> IO ()
execWithDup_variant2 ref = go
  where
    go :: Source Int () -> IO ()
    go c = let c' = dup c in \w0 -> go' c' w0

    go' :: Source Int () -> IO ()
    go' (Done ())   = \w0 -> (# w0, () #)
    go' (Yield b k) = \w0 -> let !(# w1, () #) = unwrapIO (modifyIORef' ref (+ b)) w0
                             in go k w1

{-# NOINLINE execWithDup_variant3 #-}
execWithDup_variant3 :: IORef Int -> Source Int () -> IO ()
execWithDup_variant3 ref = go
  where
    go :: Source Int () -> IO ()
    go c = \w0 ->
        let c' = dup c
        in go' c' w0

    go' :: Source Int () -> IO ()
    go' (Done ())   = \w0 -> (# w0, () #)
    go' (Yield b k) = let k' = go k
                      in \w0 -> let !(# w1, () #) = unwrapIO (modifyIORef' ref (+ b)) w0
                                in k' w1

-- | Variant 4
--
-- It is worth spelling out why this variant is OK.
--
-- o The top-level call to execWithDup_variant4 will result in the first @dup@
--   be executed. Since we pulled it out of the lambda, this will happen only
--   once: subsequent executions will /not/ duplicate this part of the conduit.
-- o However, the allocation of @go k@ in @go'@ /cannot happen/ until after
--   the pattern match. This is therefore a new thunk each time, which will
--   duplicate the (tail of the) conduit each time.
{-# NOINLINE execWithDup_variant4 #-}
execWithDup_variant4 :: IORef Int -> Source Int () -> IO ()
execWithDup_variant4 ref = go
  where
    go :: Source Int () -> IO ()
    go c = let c' = dup c in \w0 -> go' c' w0

    go' :: Source Int () -> IO ()
    go' (Done ())   = \w0 -> (# w0, () #)
    go' (Yield b k) = let k' = go k
                      in \w0 -> let !(# w1, () #) = unwrapIO (modifyIORef' ref (+ b)) w0
                                in k' w1
