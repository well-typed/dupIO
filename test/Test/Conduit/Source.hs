module Test.Conduit.Source (tests) where

import Prelude hiding (IO, (<*))

import Test.Tasty

import Data.Dup.IO

import Test.Util.TestSetup

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Conduit.Source" [
      testLocalOOM "withoutDupIO"  test_withoutDupIO
    , testCaseInfo "outerDupIO"    test_outerDupIO
    , testCaseInfo "innerDupIO"    test_innerDupIO
    ]

test_withoutDupIO :: IO String
test_withoutDupIO = \w0 ->
    let c = yieldFrom limit
        !(# w1, _sum #) = retry (computeTotal c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_outerDupIO :: IO String
test_outerDupIO = \w0 ->
    let c = yieldFrom limit
        !(# w1, _sum #) = retry (outerDupIO c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_innerDupIO :: IO String
test_innerDupIO = \w0 ->
    let c = yieldFrom limit
        !(# w1, _sum #) = retry (innerDupIO c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

{-------------------------------------------------------------------------------
  Constructing and processing sources
-------------------------------------------------------------------------------}

{-# NOINLINE yieldFrom #-}
yieldFrom :: Int -> Conduit i Int m ()
yieldFrom 0 = Done ()
yieldFrom n = Yield n $ yieldFrom (n - 1)

{-# NOINLINE computeTotal #-}
computeTotal :: Conduit i Int m () -> IO Int
computeTotal = \c w -> go 0 c w
  where
    go :: Int -> Conduit i Int m () -> IO Int
    go acc (Done ())   = \w -> (# w, acc #)
    go acc (Yield o k) = \w -> let acc' = acc + o
                               in acc' `seq` go acc' k w
    go _ _ = error "computeTotal: unexpected conduit"

{-# NOINLINE outerDupIO #-}
outerDupIO :: Conduit i Int m () -> IO Int
outerDupIO c = \w0 ->
    let !(# w1, c' #) = unwrapIO (dupIO c) w0
    in computeTotal c' w1

{-# NOINLINE innerDupIO #-}
innerDupIO :: Conduit i Int m () -> IO Int
innerDupIO = \c w -> go 0 c w
  where
    go :: Int -> Conduit i Int m () -> IO Int
    go acc c w0 =
        let !(# w1, c' #) = unwrapIO (dupIO c) w0
        in go' acc c' w1

    go' :: Int -> Conduit i Int m () -> IO Int
    go' acc (Done ())   = \w -> (# w, acc #)
    go' acc (Yield o k) = \w -> let acc' = acc + o
                                in acc' `seq` go acc' k w
    go' _ _ = error "computeTotal: unexpected conduit"
