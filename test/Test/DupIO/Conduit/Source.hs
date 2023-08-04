module Test.DupIO.Conduit.Source (tests) where

import Prelude hiding (IO, (<*))

import Data.IORef
import Test.Tasty

import qualified System.IO.Unsafe as Unsafe

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
    , testLocalOOM "sourceOuterDupIO_caf.OOM"                test_sourceOuterDupIO_caf
    , testCaseInfo "sourceInnerDupIO.OK"                     test_sourceInnerDupIO
    , testCaseInfo "sourceInnerDupIO_partiallyEvaluated.OK"  test_sourceInnerDupIO_partiallyEvaluated
    , testCaseInfo "sourceInnerDupIO_caf.OK"                 test_sourceInnerDupIO_caf
    ]

test_sourceWithoutDupIO :: IO String
test_sourceWithoutDupIO = \w0 ->
    let c = yieldFrom limit
        !(# w1, _sum #) = retry (runSource c <* checkMem (1 * mb)) w0
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

-- This test will fail, because we cannot duplicate the CAF itself (#20).
test_sourceOuterDupIO_caf :: IO String
test_sourceOuterDupIO_caf = withSingleUseCAF caf1Ref $ \caf w0 ->
    let !(# w1, _sum #) = retry (outerDupIO caf <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)

-- However, even though we cannot duplicate the first link in the chain (i.e.,
-- the CAF), that shouldn't matter if we duplicate /everything/; we'd retain
-- a tiny bit, but nothing to cause trouble.
--
-- Using a different CAF here to avoid these two tests influencing each other.
test_sourceInnerDupIO_caf :: IO String
test_sourceInnerDupIO_caf = withSingleUseCAF caf2Ref $ \caf w0 ->
    let !(# w1, _sum #) = retry (innerDupIO caf <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)

{-------------------------------------------------------------------------------
  Interpreter

  All recursion happens _inside_ the @State# Realworld ->@ lambda; see
  <https://well-typed.com/blog/2016/09/sharing-conduit/#addendum-1-ghcs-state-hack>
  for some discussion.
-------------------------------------------------------------------------------}

{-# NOINLINE caf1 #-}
caf1 :: Source Int ()
caf1 = yieldFrom limit
  where
    limit :: Int
    limit = 250_000

{-# NOINLINE caf1Ref #-}
caf1Ref :: IORef (Maybe (Source Int ()))
caf1Ref = Unsafe.unsafePerformIO $ newIORef (Just caf1)

{-# NOINLINE caf2 #-}
caf2 :: Source Int ()
caf2 = yieldFrom limit
  where
    limit :: Int
    limit = 250_000

{-# NOINLINE caf2Ref #-}
caf2Ref :: IORef (Maybe (Source Int ()))
caf2Ref = Unsafe.unsafePerformIO $ newIORef (Just caf2)

{-# NOINLINE runSource #-}
runSource :: Source Int () -> IO Int
runSource = go 0
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
    in runSource c' w1

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
