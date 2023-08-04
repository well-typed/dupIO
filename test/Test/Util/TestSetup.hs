module Test.Util.TestSetup (
    -- * Expose IO
    IO
  , wrapIO
  , unwrapIO
  , unsafePerformIO
  , evaluate
  , (<*)
  , dupIO
  , replicateM_
    -- ** In tasty
  , Assertion
  , testCase
  , testCaseInfo
  , testLocalOOM
  , assertEqualInfo
    -- * Top-level handlers
  , retry
  , withSingleUseCAF
    -- * Check memory usage
  , MemSize -- opaque
  , mb
  , checkMem
  , LocalOutOfMemoryException -- opaque
  ) where

import Prelude hiding (IO, (<*))

import Control.Exception (SomeException, Exception)
import Data.Coerce
import Data.IORef
import Data.Word
import GHC.Prim (State#, RealWorld)
import GHC.Stats
import System.Mem
import Test.Tasty (TestName, TestTree)

import qualified Control.Exception as E
import qualified Control.Monad     as Control.Monad
import qualified Data.Dup          as DupIO
import qualified GHC.IO            as GHC.IO
import qualified Test.Tasty.HUnit  as Tasty

import Test.Util

{-------------------------------------------------------------------------------
  Expose IO
-------------------------------------------------------------------------------}

type IO a = State# RealWorld -> (# State# RealWorld, a #)

wrapIO :: IO a -> GHC.IO.IO a
wrapIO = coerce

unwrapIO :: GHC.IO.IO a -> IO a
unwrapIO = coerce

unsafePerformIO :: forall a. IO a -> a
unsafePerformIO = coerce (GHC.IO.unsafePerformIO @a)

evaluate :: forall a. a -> IO a
evaluate = coerce (GHC.IO.evaluate @a)

(<*) :: forall a. IO a -> IO () -> IO a
f <* g = \w0 ->
    let !(# w1, a  #) = f w0
        !(# w2, () #) = g w1
    in (# w2, a #)

dupIO :: forall a. a -> IO a
dupIO = coerce (DupIO.dupIO @a)

{-------------------------------------------------------------------------------
  Unwrapped IO in assertions
-------------------------------------------------------------------------------}

type Assertion = IO ()

testCase :: TestName -> Assertion -> TestTree
testCase name = Tasty.testCase name . wrapIO

testCaseInfo :: TestName -> IO String -> TestTree
testCaseInfo name = Tasty.testCaseInfo name . wrapIO

-- | Like 'testCase', but succeed only if the assertion throws a
-- 'LocalOutOfMemoryException' exception
testLocalOOM :: TestName -> IO String -> TestTree
testLocalOOM name assertion = testCaseInfo name $ \w0 ->
    let !(# w1, mEx #) = unwrapIO (E.try $ wrapIO assertion) w0
    in case mEx of
         Right _ -> unwrapIO (Tasty.assertFailure failure) w1
         Left  e -> (# w1, success e #)
  where
    failure :: String
    failure =
        "did not get expected LocalOutOfMemoryException"

    success :: LocalOutOfMemoryException -> String
    success LocalOOM{live} =
        "got expected OOM (live: " ++ show live ++ " bytes)"

assertEqualInfo :: (Show a, Eq a) => String -> a -> a -> IO String
assertEqualInfo ok x y
  | x == y    = \w -> (# w, ok #)
  | otherwise = unwrapIO (E.throwIO $ userError $ show x ++ " /= " ++ show y)

{-------------------------------------------------------------------------------
  Top-level handlers
-------------------------------------------------------------------------------}

{-# NOINLINE retry #-}
retry :: forall a. IO a -> IO a
retry io = unwrapIO $ go 0
  where
    go :: Int -> GHC.IO.IO a
    go retries = do
        ma <- E.tryJust shouldCatch (wrapIO io)
        case ma of
          Right a ->
            return a
          Left e -> do
            putStrLn $ "retry: trying again after " ++ show e
            go (retries + 1)
      where
        shouldCatch :: SomeException -> Maybe SomeException
        shouldCatch e
          | retries > 0
          = Nothing

          | Just LocalOOM{} <- E.fromException e
          = Nothing

          | otherwise
          = Just e

replicateM_ :: Int -> IO () -> IO ()
replicateM_ n io = unwrapIO $ Control.Monad.replicateM_ n (wrapIO io)

-- | Ensure GC of CAF after test
--
-- Tests that involve CAFs are tricky, because the exact moment that a CAF is
-- GCed depends on all kinds of things (and it's a bit conservative). Instead we
-- access the CAF through a (global) IORef which is initialized to point to the
-- CAF, and which is cleared the moment the test completes; this ensures that
-- each test cannot affect the next.
withSingleUseCAF :: forall a b. IORef (Maybe a) -> (a -> IO b) -> IO b
withSingleUseCAF cafRef k = unwrapIO go
  where
    go :: GHC.IO.IO b
    go = do
        -- This function should only ever be called once for each CAF
        Just caf <- readIORef cafRef
        wrapIO (k caf) `E.finally` writeIORef cafRef Nothing

{-------------------------------------------------------------------------------
  Check memory usage
-------------------------------------------------------------------------------}

newtype MemSize = MemSize Word64
  deriving newtype (Eq, Ord, Num)

instance Show MemSize where
  show (MemSize sz) = addNumericUnderscores (show sz)

mb :: MemSize
mb = 1024 * 1024

checkMem :: MemSize -> IO ()
checkMem limit = unwrapIO $ do
    statsEnabled <- getRTSStatsEnabled
    if not statsEnabled then
      E.throwIO RtsStatsNotEnabled
    else do
      performMajorGC
      stats <- getRTSStats
      let live = MemSize $ gcdetails_live_bytes (gc stats)
      if live > limit
        then E.throwIO LocalOOM{live, limit}
        else return ()

data LocalOutOfMemoryException = LocalOOM {
      live  :: MemSize
    , limit :: MemSize
    }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | The tests depend on the @-T@ RTS flag
--
-- <https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-options-to-produce-runtime-statistics>
data RtsStatsNotEnabled = RtsStatsNotEnabled
  deriving stock (Show)
  deriving anyclass (Exception)
