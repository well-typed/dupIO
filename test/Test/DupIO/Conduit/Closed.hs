module Test.DupIO.Conduit.Closed (tests) where

import Prelude hiding (IO, (<*))
import qualified Prelude

import Data.IORef
import Test.Tasty

import qualified System.IO.Unsafe as Unsafe

import Test.Util.Conduit.Closed
import Test.Util.TestSetup

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.DupIO.Conduit.Closed" [
      testLocalOOM "closedWithoutDupIO.OOM"  test_closedWithoutDupIO
    , testCaseInfo "closedInnerDupIO.OK"     test_closedInnerDupIO
    , testCaseInfo "closedInnerDupIO_caf.OK" test_closedInnerDupIO_caf
    ]

test_closedWithoutDupIO :: IO String
test_closedWithoutDupIO = \w0 ->
    let !(# w1, ref #) = unwrapIO (newIORef 0) w0
        c = addFrom ref limit
        !(# w2, () #) = retry (runConduit c <* checkMem (1 * mb)) w1
    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 100_000

test_closedInnerDupIO :: IO String
test_closedInnerDupIO = \w0 ->
    let !(# w1, ref #) = unwrapIO (newIORef 0) w0
        c = addFrom ref limit
        !(# w2, () #) = retry (innerDupIO c <* checkMem (1 * mb)) w1
    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 100_000

test_closedInnerDupIO_caf :: IO String
test_closedInnerDupIO_caf = withSingleUseCAF caf1Ref $ \caf w0 ->
    let !(# w1, () #) = retry (innerDupIO caf <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

{-# NOINLINE globalIORef #-}
globalIORef :: IORef Int
globalIORef = Unsafe.unsafePerformIO $ newIORef 0

{-# NOINLINE caf1 #-}
caf1 :: Closed Prelude.IO ()
caf1 = addFrom globalIORef limit
  where
    limit :: Int
    limit = 100_000

{-# NOINLINE caf1Ref #-}
caf1Ref :: IORef (Maybe (Closed Prelude.IO ()))
caf1Ref = Unsafe.unsafePerformIO $ newIORef (Just caf1)

{-# NOINLINE runConduit #-}
runConduit :: Closed Prelude.IO () -> IO ()
runConduit = go
  where
    go :: Closed Prelude.IO () -> IO ()
    go (Done r) = \w0 -> (# w0, r #)
    go (Lift k) = \w0 -> let !(# w1, c #) = unwrapIO k w0
                         in go c w1

{-# NOINLINE innerDupIO #-}
innerDupIO :: Closed Prelude.IO () -> IO ()
innerDupIO = go
  where
    go :: Closed Prelude.IO () -> IO ()
    go c = \w0 ->
        let !(# w1, c' #) = dupIO c w0
        in go' c' w1

    go' :: Closed Prelude.IO () -> IO ()
    go' (Done r) = \w0 -> (# w0, r #)
    go' (Lift k) = \w0 -> let !(# w1, c #) = unwrapIO k w0
                          in go c w1
