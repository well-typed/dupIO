module Test.Conduit.Source (tests) where

import Prelude hiding (IO, (<*))

import Data.Functor.Identity
import Test.Tasty

import Data.Dup.IO

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
tests = testGroup "Test.Conduit.Source" [
      testLocalOOM "OOM.withoutDupIO" test_withoutDupIO
    , testCaseInfo "OK.outerDupIO"    test_outerDupIO
    , testCaseInfo "OK.innerDupIO"    test_innerDupIO
--    , testCaseInfo "OK.cafWithDupIO"  test_cafWithDupIO
    ]

test_withoutDupIO :: IO String
test_withoutDupIO = \w0 ->
    let c = yieldFrom limit
        !(# w1, _sum #) = retry (runConduit c <* checkMem (1 * mb)) w0
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

test_cafWithDupIO :: IO String
test_cafWithDupIO = \w0 ->
    let !(# w1, _sum #) = retry (outerDupIO caf <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)

{-------------------------------------------------------------------------------
  Constructing and processing sources

  All recursion happens _inside_ the @State# Realworld ->@ lambda; see
  <https://well-typed.com/blog/2016/09/sharing-conduit/#addendum-1-ghcs-state-hack>
  for some discussion.
-------------------------------------------------------------------------------}

{-# NOINLINE caf #-}
caf :: Conduit () Int Identity ()
caf = yieldFrom limit
  where
    limit :: Int
    limit = 250_000

{-# NOINLINE yieldFrom #-}
yieldFrom :: Int -> Conduit i Int m ()
yieldFrom 0 = Done ()
yieldFrom n = Yield n $ yieldFrom (n - 1)

{-# NOINLINE runConduit #-}
runConduit :: Conduit i Int m () -> IO Int
runConduit = go 0
  where
    go :: Int -> Conduit i Int m () -> IO Int
    go acc (Done ())   = \w -> (# w, acc #)
    go acc (Yield o k) = \w -> let acc' = acc + o
                               in acc' `seq` go acc' k w
    go _ _ = error "runConduit: unexpected conduit"

{-# NOINLINE outerDupIO #-}
outerDupIO :: Conduit i Int m () -> IO Int
outerDupIO c = \w0 ->
    let !(# w1, c' #) = unwrapIO (dupIO c) w0
    in runConduit c' w1

{-# NOINLINE innerDupIO #-}
innerDupIO :: Conduit i Int m () -> IO Int
innerDupIO = go 0
  where
    go :: Int -> Conduit i Int m () -> IO Int
    go acc c w0 =
        let !(# w1, c' #) = unwrapIO (dupIO c) w0
        in go' acc c' w1

    go' :: Int -> Conduit i Int m () -> IO Int
    go' acc (Done ())   = \w -> (# w, acc #)
    go' acc (Yield o k) = \w -> let acc' = acc + o
                                in acc' `seq` go acc' k w
    go' _ _ = error "runConduit: unexpected conduit"
