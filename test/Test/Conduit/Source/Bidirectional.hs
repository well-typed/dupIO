module Test.Conduit.Source.Bidirectional (tests) where

import Prelude hiding (IO, (<*))

import Test.Tasty

import Test.Util.Conduit.Source.Bidirectional
import Test.Util.TestSetup

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Conduit.Source.Bidirectional" [
      testCaseInfo "innerDupIO_partiallyEvaluated.OK"  test_innerDupIO_partiallyEvaluated
    ]

test_innerDupIO_partiallyEvaluated :: IO String
test_innerDupIO_partiallyEvaluated = \w0 ->
    let c               = yieldFrom limit
        !(# w1, c'   #) = evaluate c                                 w0
        !(# w2, _sum #) = retry (innerDupIO c' <* checkMem (1 * mb)) w1
    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

{-------------------------------------------------------------------------------
  Constructing and processing sources
-------------------------------------------------------------------------------}

{-# NOINLINE yieldFrom #-}
yieldFrom :: Int -> Source () Int ()
yieldFrom 0 = Done ()
yieldFrom n = let k = yieldFrom (n - 1)
              in Yield n $ \() -> k

{-# NOINLINE innerDupIO #-}
innerDupIO :: Source () Int () -> IO Int
innerDupIO = go 0
  where
    go :: Int -> Source () Int () -> IO Int
    go acc c = \w0 ->
        let !(# w1, c' #) = dupIO c w0
        in go' acc c' w1

    go' :: Int -> Source () Int () -> IO Int
    go' acc (Done ()) = \w0 ->
        (# w0, acc #)
    go' acc (Yield b k) = \w0 ->
        let !acc'         = acc + b
            !(# w1, k' #) = dupIO k w0
        in go acc' (k' ()) w1

