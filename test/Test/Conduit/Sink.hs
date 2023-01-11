module Test.Conduit.Sink (tests) where

import Prelude hiding (IO, (<*))

import Data.Functor.Identity
import Data.Void
import Test.Tasty

import Data.Dup.IO

import Test.Util.TestSetup

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Conduit.Sink" [
      testLocalOOM "OOM.withoutDupIO" test_withoutDupIO
    , testCaseInfo "OK.outerDupIO"    test_outerDupIO
    , testCaseInfo "OK.innerDupIO"    test_innerDupIO
--    , testCaseInfo "OK.cafWithDupIO"  test_cafWithDupIO
    ]

test_withoutDupIO :: IO String
test_withoutDupIO = \w0 ->
    let c = countChars 0
        !(# w1, _count #) = retry (runConduit limit 'a' c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_outerDupIO :: IO String
test_outerDupIO = \w0 ->
    let c = countChars 0
        !(# w1, _count #) = retry (outerDupIO limit 'a' c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_innerDupIO :: IO String
test_innerDupIO = \w0 ->
    let c = countChars 0
        !(# w1, _count #) = retry (innerDupIO limit 'a' c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_cafWithDupIO :: IO String
test_cafWithDupIO = \w0 ->
    let !(# w1, _count #) = retry (outerDupIO limit 'a' caf <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

{-------------------------------------------------------------------------------
  Constructing and processing sinks

  The strange way that 'countChars' is written is modelling what
  full laziness may very well do to your conduit.
-------------------------------------------------------------------------------}

{-# NOINLINE caf #-}
caf :: Conduit Char Void Identity Int
caf = countChars 0

{-# NOINLINE countChars #-}
countChars :: Int -> Conduit Char o m Int
countChars cnt =
    let next = let cnt' = cnt + 1
               in cnt' `seq` countChars cnt'
    in Await $ \mi -> case mi of
         Left  _ -> Done cnt
         Right _ -> next

{-# NOINLINE runConduit #-}
runConduit :: Int -> Char -> Conduit Char o m Int -> IO Int
runConduit limit ch =
    go limit
  where
    go :: Int -> Conduit Char o m Int -> IO Int
    go _ (Done r)  = \w -> (# w, r #)
    go 0 (Await k) = \w -> go 0     (k (Left 0))   w
    go n (Await k) = \w -> go (n-1) (k (Right ch)) w
    go _ _ = error "runConduit: unexpected conduit"

{-# NOINLINE outerDupIO #-}
outerDupIO :: Int -> Char -> Conduit Char o m Int -> IO Int
outerDupIO limit ch c = \w0 ->
    let !(# w1, c' #) = unwrapIO (dupIO c) w0
    in runConduit limit ch c' w1

{-# NOINLINE innerDupIO #-}
innerDupIO :: Int -> Char -> Conduit Char o m Int -> IO Int
innerDupIO limit ch =
    go limit
  where
    go :: Int -> Conduit Char o m Int -> IO Int
    go n c w0 =
        let !(# w1, c' #) = unwrapIO (dupIO c) w0
        in go' n c' w1

    go' :: Int -> Conduit Char o m Int -> IO Int
    go' _ (Done r)  = \w -> (# w, r #)
    go' 0 (Await k) = \w -> go 0     (k (Left 0))   w
    go' n (Await k) = \w -> go (n-1) (k (Right ch)) w
    go' _ _ = error "runConduit: unexpected conduit"
