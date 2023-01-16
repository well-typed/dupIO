module Test.Dup.Conduit.Sink (tests) where

import Prelude hiding (IO, (<*))

import Data.Dup.IO

import Test.Tasty

import Test.Util.TestSetup
import Test.Util.Conduit.Sink

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Dup.Conduit.Sink" [
      testLocalOOM "withoutDup.OOM" test_withoutDup
    , testCaseInfo "innerDup.OK"    test_innerDup
    ]

test_withoutDup :: IO String
test_withoutDup = \w0 ->
    let c = countChars 0

        runConduit :: IO Int
        runConduit = \w' -> let result = evalConduit limit 'a' c
                            in evaluate result w'

        !(# w1, _count #) = retry (runConduit <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_innerDup :: IO String
test_innerDup = \w0 ->
    let c = countChars 0

        runConduit :: IO Int
        runConduit = \w' -> let result = innerDup limit 'a' c
                            in evaluate result w'

        !(# w1, _count #) = retry (runConduit <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

{-# NOINLINE evalConduit #-}
evalConduit :: Int -> Char -> Sink (Maybe Char) Int -> Int
evalConduit limit ch =
    go limit
  where
    go :: Int -> Sink (Maybe Char) Int -> Int
    go _ (Done r)  = r
    go 0 (Await k) = go 0     (unbox $ k Nothing)
    go n (Await k) = go (n-1) (unbox $ k (Just ch))

{-# NOINLINE innerDup #-}
innerDup :: Int -> Char -> Sink (Maybe Char) Int -> Int
innerDup limit ch =
    go limit
  where
    go :: Int -> Sink (Maybe Char) Int -> Int
    go n c =
        case dup c of
          Done  r -> r
          Await k ->
            if n == 0
              then case k Nothing   of Box k' -> go 0       k'
              else case k (Just ch) of Box k' -> go (n - 1) k'