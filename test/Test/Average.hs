module Test.Average (tests) where

import Prelude hiding (IO, sum)

import Data.Foldable (foldl')
import Test.Tasty

import Test.Util.TestSetup

tests :: TestTree
tests = testGroup "Test.Average" [
      testLocalOOM "withoutDupIO.OOM"       test_withoutDupIO
    , testCaseInfo "withDupIO.OK"           test_withDupIO
    , testLocalOOM "partiallyEvaluated.OOM" test_partiallyEvaluated
    ]

test_withoutDupIO :: IO String
test_withoutDupIO = \w0 ->
    let xs, ys :: [Int]
        xs = [1 .. 250_000]
        ys = [1 .. 250_000] -- used to compute reference result

        !(# w1, ref #) = evaluate (reference    ys) w0
        !(# w2, avg #) = evaluate (withoutDupIO xs) w1

    in assertEqualInfo "succeeded with 1MB memory limit" ref avg w2

test_withDupIO :: IO String
test_withDupIO = \w0 ->
    let xs, ys :: [Int]
        xs = [1 .. 250_000]
        ys = [1 .. 250_000] -- used to compute reference result

        !(# w1, ref #) = evaluate (reference ys) w0
        !(# w2, avg #) = withDupIO xs            w1

    in assertEqualInfo "succeeded with 1MB memory limit" ref avg w2

-- | Test with partially evaluated list
--
-- Demonstrate that once we force the first cell of the list to WHNF,
-- shallow duplication is insufficient (we need to duplicate each time we
-- pattern match).
test_partiallyEvaluated :: IO String
test_partiallyEvaluated = \w0 ->
    let xs, ys :: [Int]
        xs = [1 .. 250_000]
        ys = [1 .. 250_000] -- used to compute reference result

        !(# w1, ref #) = evaluate (reference ys) w0
        !(# w2, xs' #) = evaluate xs             w1
        !(# w3, avg #) = withDupIO xs'           w2

    in assertEqualInfo "succeeded with 1MB memory limit" ref avg w3

{-------------------------------------------------------------------------------
  Versions of 'average'
-------------------------------------------------------------------------------}

reference :: [Int] -> Int
reference = go 0 0
  where
    go :: Int -> Int -> [Int] -> Int
    go !sum !len []     = sum `div` len
    go !sum !len (x:xs) = go (sum + x) (len + 1) xs

withoutDupIO :: [Int] -> Int
withoutDupIO xs = unsafePerformIO $ \w0 ->
    let !(# w1, sum #) = evaluate (foldl' (+) 0 xs) w0
        !(# w2, ()  #) = checkMem 1_000_000         w1
        !(# w3, len #) = evaluate (length xs)       w2
    in (# w3, sum `div` len #)

withDupIO :: [Int] -> IO Int
withDupIO xs = \w0 ->
    let !(# w1, xs' #) = dupIO xs                   w0
        !(# w2, sum #) = evaluate (foldl' (+) 0 xs) w1
        !(# w3, ()  #) = checkMem 1_000_000         w2
        !(# w4, len #) = evaluate (length xs')      w3
    in (# w4, sum `div` len #)