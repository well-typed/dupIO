module Test.Sanity.DupIO (tests) where

import Control.Exception
import Data.IORef
import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.HUnit

import Data.Dup.IO

tests :: TestTree
tests = testGroup "Test.Sanity.DupIO" [
      testCase "observeEvaluation.OK" test_observeEvaluation
    ]

test_observeEvaluation :: Assertion
test_observeEvaluation = do
    ref :: IORef Int <- newIORef 0

    let thunk :: ()
        thunk = unsafePerformIO $ modifyIORef ref (+ 1)

    evaluate =<< dupIO thunk
    evaluate =<< dupIO thunk

    x <- readIORef ref
    assertEqual "" 2 x


