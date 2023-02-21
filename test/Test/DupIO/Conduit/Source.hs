module Test.DupIO.Conduit.Source (tests) where

import Prelude hiding (IO, (<*))

import Test.Tasty

import Test.Util.Conduit.Source
import Test.Util.TestSetup

{-------------------------------------------------------------------------------
  Tests

  The conduit is allocated /inside/ the scope of the test, thereby guaranteeing
  that it can be GCed once the test completes; this is important, because it
  means that a memcheck in one test does not affect another.

  The conduit is allocated /outside/ the scope of the retry, so that @retry@
  holds on to the action (in case it needs to retry it), which will then hold
  on to the conduit in turn, thereby resulting in the memory leak.
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.DupIO.Conduit.Source" [
      testLocalOOM "sourceWithoutDupIO.OOM"                  test_sourceWithoutDupIO
    , testCaseInfo "sourceOuterDupIO.OK"                     test_sourceOuterDupIO
    , testLocalOOM "sourceOuterDupIO_partiallyEvaluated.OOM" test_sourceOuterDupIO_partiallyEvaluated
    , testCaseInfo "sourceInnerDupIO.OK"                     test_sourceInnerDupIO
    , testCaseInfo "sourceInnerDupIO_partiallyEvaluated.OK"  test_sourceInnerDupIO_partiallyEvaluated
    , testLocalOOM "sourceCafWithoutDupIO.OOM"               test_sourceCafWithoutDupIO
    ]

test_sourceWithoutDupIO :: IO String
test_sourceWithoutDupIO = \w0 ->
    let c = yieldFrom limit
        !(# w1, _sum #) = retry (runSourceConduit c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_sourceOuterDupIO :: IO String
test_sourceOuterDupIO = \w0 ->
    let c = yieldFrom limit
        !(# w1, _sum #) = retry (outerDupIO c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_sourceOuterDupIO_partiallyEvaluated :: IO String
test_sourceOuterDupIO_partiallyEvaluated = \w0 ->
    let c = yieldFrom limit
        !(# w1, c'   #) = evaluate c                                 w0
        !(# w2, _sum #) = retry (outerDupIO c' <* checkMem (1 * mb)) w1
    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_sourceInnerDupIO :: IO String
test_sourceInnerDupIO = \w0 ->
    let c = yieldFrom limit
        !(# w1, _sum #) = retry (innerDupIO c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_sourceInnerDupIO_partiallyEvaluated :: IO String
test_sourceInnerDupIO_partiallyEvaluated = \w0 ->
    let c = yieldFrom limit
        !(# w1, c'   #) = evaluate c                                 w0
        !(# w2, _sum #) = retry (innerDupIO c' <* checkMem (1 * mb)) w1
    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_sourceCafWithoutDupIO :: IO String
test_sourceCafWithoutDupIO = \w0 ->
    let !(# w1, _sum #) = (runSourceConduit caf <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)

{-------------------------------------------------------------------------------
  Interpreter

  All recursion happens _inside_ the @State# Realworld ->@ lambda; see
  <https://well-typed.com/blog/2016/09/sharing-conduit/#addendum-1-ghcs-state-hack>
  for some discussion.
-------------------------------------------------------------------------------}

{-# NOINLINE caf #-}
caf :: Source Int ()
caf = yieldFrom limit
  where
    limit :: Int
    limit = 250_000

{-# NOINLINE runSourceConduit #-}
runSourceConduit :: Source Int () -> IO Int
runSourceConduit = go 0
  where
    go :: Int -> Source Int () -> IO Int
    go acc (Done ()) = \w0 ->
        (# w0, acc #)
    go acc (Yield b k) = \w0 ->
        let !acc' = acc + b
        in go acc' k w0

{-# NOINLINE outerDupIO #-}
outerDupIO :: Source Int () -> IO Int
outerDupIO c = \w0 ->
    let !(# w1, c' #) = dupIO c w0
    in runSourceConduit c' w1

{-# NOINLINE innerDupIO #-}
innerDupIO :: Source Int () -> IO Int
innerDupIO = go 0
  where
    go :: Int -> Source Int () -> IO Int
    go acc c w0 =
        let !(# w1, c' #) = dupIO c w0
        in go' acc c' w1

    go' :: Int -> Source Int () -> IO Int
    go' acc (Done ()) = \w0 ->
        (# w0, acc #)
    go' acc (Yield b k) = \w0 ->
        let !acc' = acc + b
        in go acc' k w0
