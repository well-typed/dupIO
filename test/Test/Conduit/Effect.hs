module Test.Conduit.Effect (tests) where

import Prelude hiding (IO, (<*))
import qualified Prelude

import Data.IORef
import Data.Void
import Test.Tasty

import qualified System.IO.Unsafe as Unsafe

import Data.Dup.IO

import Test.Util.TestSetup

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Conduit.Effect" [
      testLocalOOM "OOM.withoutDupIO" test_withoutDupIO
    , testCaseInfo "OK.outerDupIO"    test_outerDupIO
    , testCaseInfo "OK.innerDupIO"    test_innerDupIO
--    , testCaseInfo "OK.cafWithDupIO"  test_cafWithDupIO
    ]

test_withoutDupIO :: IO String
test_withoutDupIO = \w0 ->
    let !(# w1, ref #) = unwrapIO (newIORef 0) w0
        c = addFrom ref limit
        !(# w2, () #) = retry (runConduit c <* checkMem (1 * mb)) w1
    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 100_000

test_outerDupIO :: IO String
test_outerDupIO = \w0 ->
    let !(# w1, ref #) = unwrapIO (newIORef 0) w0
        c = addFrom ref limit
        !(# w2, () #) = retry (outerDupIO c <* checkMem (1 * mb)) w1
    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 100_000

test_innerDupIO :: IO String
test_innerDupIO = \w0 ->
    let !(# w1, ref #) = unwrapIO (newIORef 0) w0
        c = addFrom ref limit
        !(# w2, () #) = retry (innerDupIO c <* checkMem (1 * mb)) w1
    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 100_000

test_cafWithDupIO :: IO String
test_cafWithDupIO = \w0 ->
    let !(# w1, () #) = retry (outerDupIO caf <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)

{-------------------------------------------------------------------------------
  Constructing and processing conduits with effects
-------------------------------------------------------------------------------}

{-# NOINLINE globalIORef #-}
globalIORef :: IORef Int
globalIORef = Unsafe.unsafePerformIO $ newIORef 0

{-# NOINLINE caf #-}
caf :: Conduit () Void Prelude.IO ()
caf = addFrom globalIORef limit
  where
    limit :: Int
    limit = 100_000

{-# NOINLINE addFrom #-}
addFrom :: IORef Int -> Int -> Conduit i o Prelude.IO ()
addFrom ref = go
  where
    go :: Int -> Conduit i o Prelude.IO ()
    go 0 = Done ()
    go n = Effect $ modifyIORef' ref (+ n) >> return (go (n - 1))

{-# NOINLINE runConduit #-}
runConduit :: Conduit i o Prelude.IO () -> IO ()
runConduit = go
  where
    go :: Conduit i o Prelude.IO () -> IO ()
    go (Done r)   = \w0 -> (# w0, r #)
    go (Effect k) = \w0 -> let !(# w1, c #) = unwrapIO k w0
                           in go c w1
    go _ = error "runPIpe: unexpected conduit"

{-# NOINLINE outerDupIO #-}
outerDupIO :: Conduit i o Prelude.IO () -> IO ()
outerDupIO c = \w0 ->
    let !(# w1, c' #) = unwrapIO (dupIO c) w0
    in runConduit c' w1

{-# NOINLINE innerDupIO #-}
innerDupIO :: Conduit i o Prelude.IO () -> IO ()
innerDupIO = go
  where
    go :: Conduit i o Prelude.IO () -> IO ()
    go c w0 =
        let !(# w1, c' #) = unwrapIO (dupIO c) w0
        in go' c' w1

    go' :: Conduit i o Prelude.IO () -> IO ()
    go' (Done r)   = \w0 -> (# w0, r #)
    go' (Effect k) = \w0 -> let !(# w1, c #) = unwrapIO k w0
                            in go c w1
    go' _ = error "runPIpe: unexpected conduit"
