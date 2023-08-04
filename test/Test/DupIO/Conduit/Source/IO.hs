module Test.DupIO.Conduit.Source.IO (tests) where

import Prelude hiding (IO, (<*))

import Test.Tasty

import Test.Util.TestSetup
import Test.Util.Conduit.Source.IO

{-------------------------------------------------------------------------------
  Tests

  This set of tests demonstrates that baking @IO@ right into conduit itself
  doesn't really help. Or rather, it /can/ help, but each and every conduit
  needs to be defind very carefully, putting too much responsibility in the
  hands of the user.
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.DupIO.Conduit.Source.IO" [
      testCaseInfo "sourceWithoutDUPIO_variant1.OK"  test_sourceWithoutDupIO_variant1
    , testLocalOOM "sourceWithoutDUPIO_variant2.OOM" test_sourceWithoutDupIO_variant2
    , testCaseInfo "sourceWithoutDUPIO_variant3.OK"  test_sourceWithoutDupIO_variant3
    , testCaseInfo "sourceInnerDupIO_variant2.OK"    test_sourceInnerDupIO_variant2
    ]

test_sourceWithoutDupIO_variant1 :: IO String
test_sourceWithoutDupIO_variant1 = \w0 ->
    let c = yieldFrom1 limit
        !(# w1, _sum #) = retry (runConduit c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_sourceWithoutDupIO_variant2 :: IO String
test_sourceWithoutDupIO_variant2 = \w0 ->
    let c = yieldFrom2 limit
        !(# w1, _sum #) = retry (runConduit c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_sourceWithoutDupIO_variant3 :: IO String
test_sourceWithoutDupIO_variant3 = \w0 ->
    let c = yieldFrom3 limit
        !(# w1, _sum #) = retry (runConduit c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_sourceInnerDupIO_variant2 :: IO String
test_sourceInnerDupIO_variant2 = \w0 ->
    let c = yieldFrom2 limit
        !(# w1, _sum #) = retry (innerDupIO c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

{-# NOINLINE runConduit #-}
runConduit :: Source Int () -> IO Int
runConduit = go 0
  where
    go :: Int -> Source Int () -> IO Int
    go acc src = \w0 ->
        let !(# w1, step #) = alloc src w0
        in go' acc step w1

    go' :: Int -> Step Int () -> IO Int
    go' acc (Done ())   = \w -> (# w, acc #)
    go' acc (Yield b k) = \w -> let !acc' = acc + b
                                 in go acc' k w


{-# NOINLINE innerDupIO #-}
innerDupIO :: Source Int () -> IO Int
innerDupIO = go 0
  where
    go :: Int -> Source Int () -> IO Int
    go acc src = \w0 ->
        let !(# w1, src' #) = dupIO src  w0
            !(# w2, step #) = alloc src' w1
        in go' acc step w2

    go' :: Int -> Step Int () -> IO Int
    go' acc (Done ())   = \w -> (# w, acc #)
    go' acc (Yield b k) = \w -> let !acc' = acc + b
                                 in go acc' k w

