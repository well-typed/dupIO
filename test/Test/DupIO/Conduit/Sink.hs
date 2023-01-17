module Test.DupIO.Conduit.Sink (tests) where

import Prelude hiding (IO, (<*))

import Test.Tasty

import Test.Util.Conduit.Sink
import Test.Util.TestSetup

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.DupIO.Conduit.Sink" [
      testLocalOOM "withoutDupIO.OOM"                 test_withoutDupIO
    , testCaseInfo "innerDupIO.OK"                    test_innerDupIO
    , testCaseInfo "innerDupIO_partiallyEvaluated.OK" test_innerDupIO_partiallyEvaluated
--    , testCaseInfo "OK.cafWithDupIO"  test_cafWithDupIO
    ]

test_withoutDupIO :: IO String
test_withoutDupIO = \w0 ->
    let c                 = countChars 0
        !(# w1, _count #) = retry (runConduit limit 'a' c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_innerDupIO :: IO String
test_innerDupIO = \w0 ->
    let c                 = countChars 0
        !(# w1, _count #) = retry (innerDupIO limit 'a' c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_innerDupIO_partiallyEvaluated :: IO String
test_innerDupIO_partiallyEvaluated = \w0 ->
    let c                 = countChars 0
        !(# w1, c'     #) = evaluate c                                           w0
        !(# w2, _count #) = retry (innerDupIO limit 'a' c' <* checkMem (1 * mb)) w1
    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

_test_cafWithDupIO :: IO String
_test_cafWithDupIO = \w0 ->
    let !(# w1, _count #) = retry (innerDupIO limit 'a' caf <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

{-------------------------------------------------------------------------------
  Interpreter

  See "Test.Conduit.Source.Bidirectional" for a discussion of @Box@.
-------------------------------------------------------------------------------}

{-# NOINLINE caf #-}
caf :: Sink (Maybe Char) Int
caf = countChars 0

{-# NOINLINE runConduit #-}
runConduit :: Int -> Char -> Sink (Maybe Char) Int -> IO Int
runConduit limit ch =
    go limit
  where
    go :: Int -> Sink (Maybe Char) Int -> IO Int
    go _ (Done r)  = \w0 -> (# w0, r #)
    go 0 (Await k) = \w0 -> go 0     (unbox $ k Nothing)   w0
    go n (Await k) = \w0 -> go (n-1) (unbox $ k (Just ch)) w0

{-# NOINLINE innerDupIO #-}
innerDupIO :: Int -> Char -> Sink (Maybe Char) Int -> IO Int
innerDupIO limit ch =
    go limit
  where
    go :: Int -> Sink (Maybe Char) Int -> IO Int
    go n c w0 =
        let !(# w1, c' #) = dupIO c w0
        in go' n c' w1

    go' :: Int -> Sink (Maybe Char) Int -> IO Int
    go' _ (Done r)  = \w0 -> (# w0, r #)
    go' 0 (Await k) = \w0 -> case k Nothing   of Box k' -> go 0     k' w0
    go' n (Await k) = \w0 -> case k (Just ch) of Box k' -> go (n-1) k' w0
