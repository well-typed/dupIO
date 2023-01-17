module Test.DupIO.Conduit.Source.Bidirectional (tests) where

import Prelude hiding (IO, (<*))

import Test.Tasty

import Test.Util.Conduit.Source.Bidirectional
import Test.Util.TestSetup

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.DupIO.Conduit.Source.Bidirectional" [
      testCaseInfo "innerDupIO_partiallyEvaluated.OK"  test_innerDupIO_partiallyEvaluated
    ]

test_innerDupIO_partiallyEvaluated :: IO String
test_innerDupIO_partiallyEvaluated = \w0 ->
    let c               = yieldFrom limit
        !(# w1, c'   #) = evaluate c                                 w0
        !(# w2, _sum #) = retry (innerDupIO c' <* checkMem (1 * mb)) w1
    in (# w2, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

{-------------------------------------------------------------------------------
  Interpreter

  NOTE: The need for Box here is very subtle. Without it, the case for @Yield@
  in @go'@ in @innerDupIO@ would look like

  > let !acc' = acc + b
  > in go acc' (k' ()) w1

  but if we do that, then the closure that @go@ is duplicating is the closure
  @k' ()@, /NOT/ the closure that is the next source. The @Box@ wrapper gives
  us a handle that allows us to unfold this computation just enough that we
  are duplicating the right thing.

  TODO: This feels fragile, but I can't quite oversee quite how fragile. What is
  the exact invariant we need? How easy is it to get this wrong?
-------------------------------------------------------------------------------}

{-# NOINLINE innerDupIO #-}
innerDupIO :: Source () Int () -> IO Int
innerDupIO = go 0
  where
    go :: Int -> Source () Int () -> IO Int
    go acc c = \w0 ->
        let !(# w1, c' #) = dupIO c w0
        in go' acc c' w1

    go' :: Int -> Source () Int () -> IO Int
    go' acc (Done ()) = \w0 ->
        (# w0, acc #)
    go' acc (Yield b k) = \w0 ->
        let !acc' = acc + b
        in case k () of
             Box k' -> go acc' k' w0

