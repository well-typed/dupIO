module Test.DupIO.UnsafeInterleaveIO (tests) where

import Control.Exception
import Data.IORef
import System.IO.Unsafe (unsafeInterleaveIO)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Dup

tests :: TestTree
tests = testGroup "Test.DupIO.UnsafeInterleaveIO" [
      testCase "observeEvaluation.OK" test_observeEvaluation
    ]

test_observeEvaluation :: Assertion
test_observeEvaluation = do
    ref   :: IORef Int <- newIORef 0
    thunk :: ()        <- unsafeInterleaveIO $ modifyIORef ref (+ 1)

    evaluate =<< dupIO thunk
    evaluate =<< dupIO thunk

    x <- readIORef ref
    assertEqual "" 2 x
