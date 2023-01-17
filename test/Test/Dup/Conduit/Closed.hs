module Test.Dup.Conduit.Closed (tests) where

import Prelude hiding (IO, (<*))
import qualified Prelude

import Data.IORef
import Test.Tasty

import Data.Dup (dup)

import Test.Util.TestSetup
import Test.Util.Conduit.Closed

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Dup.Conduit.Closed" [
      testCaseInfo "innerDup_variant1" test_innerDup_variant1
    , testCaseInfo "innerDup_variant2" test_innerDup_variant2
    ]

test_innerDup_variant1 :: IO String
test_innerDup_variant1 = \w0 ->
    let !(# w1, ref #) = unwrapIO (newIORef 0) w0
        c = addFrom ref limit
        !(# w2, () #) = retry (innerDup_variant1 c <* checkMem (1 * mb)) w1
    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 100_000

test_innerDup_variant2 :: IO String
test_innerDup_variant2 = \w0 ->
    let !(# w1, ref #) = unwrapIO (newIORef 0) w0
        c = addFrom ref limit
        !(# w2, () #) = retry (innerDup_variant2 c <* checkMem (1 * mb)) w1
    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 100_000

{-------------------------------------------------------------------------------
  Interpreter

  We define two variants, to test whether the precise location of the call to
  'dup' matters.
-------------------------------------------------------------------------------}

{-# NOINLINE innerDup_variant1 #-}
innerDup_variant1 :: Closed Prelude.IO () -> IO ()
innerDup_variant1 = go
  where
    go :: Closed Prelude.IO () -> IO ()
    go c = \w0 ->
        let c' = dup c
        in go' c' w0

    go' :: Closed Prelude.IO () -> IO ()
    go' (Done r) = \w0 -> (# w0, r #)
    go' (Lift k) = \w0 -> let !(# w1, c #) = unwrapIO k w0
                          in go c w1

{-# NOINLINE innerDup_variant2 #-}
innerDup_variant2 :: Closed Prelude.IO () -> IO ()
innerDup_variant2 = go
  where
    go :: Closed Prelude.IO () -> IO ()
    go c = let c' = dup c in \w0 -> go' c' w0

    go' :: Closed Prelude.IO () -> IO ()
    go' (Done r) = \w0 -> (# w0, r #)
    go' (Lift k) = \w0 -> let !(# w1, c #) = unwrapIO k w0
                          in go c w1
