module Test.Conduit.Source.IO (tests) where

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
tests = testGroup "Test.Conduit.Source.IO" [
      testCaseInfo "withoutDUPIO_variant1.OK"  test_withoutDupIO_variant1
    , testLocalOOM "withoutDUPIO_variant2.OOM" test_withoutDupIO_variant2
    , testCaseInfo "withoutDUPIO_variant3.OK"  test_withoutDupIO_variant3
    , testCaseInfo "innerDupIO.variant2_OK"    test_innerDupIO_variant2
    ]

test_withoutDupIO_variant1 :: IO String
test_withoutDupIO_variant1 = \w0 ->
    let c = yieldFrom1 limit
        !(# w1, _sum #) = retry (runConduit c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_withoutDupIO_variant2 :: IO String
test_withoutDupIO_variant2 = \w0 ->
    let c = yieldFrom2 limit
        !(# w1, _sum #) = retry (runConduit c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_withoutDupIO_variant3 :: IO String
test_withoutDupIO_variant3 = \w0 ->
    let c = yieldFrom3 limit
        !(# w1, _sum #) = retry (runConduit c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

test_innerDupIO_variant2 :: IO String
test_innerDupIO_variant2 = \w0 ->
    let c = yieldFrom2 limit
        !(# w1, _sum #) = retry (innerDupIO c <* checkMem (1 * mb)) w0
    in (# w1, "succeeded with 1MB memory limit" #)
  where
    limit :: Int
    limit = 250_000

{-------------------------------------------------------------------------------
  Constructing and processing sources
-------------------------------------------------------------------------------}

-- | Version 1
--
-- This is probably the most obvious version. It corresponds to:
--
-- > yieldFrom1 :: Int -> Source Int ()
-- > yieldFrom1 0 = Alloc $ return $ Done ()
-- > yieldFrom1 n = Alloc $ return $ Yield n (yieldFrom1 (n - 1))
--
-- This version is OK, but only if ghc doesn't modify it (see version2).
{-# NOINLINE yieldFrom1 #-}
yieldFrom1 :: Int -> Source Int ()
yieldFrom1 0 = Alloc $ \w -> (# w, Done ()                      #)
yieldFrom1 n = Alloc $ \w -> (# w, Yield n (yieldFrom1 (n - 1)) #)

-- | Version 2
--
-- The problem with version 1 is that ghc could easily turn it into version 2,
-- which corresponds to:
--
-- > yieldFrom2 :: Int -> Source Int ()
-- > yieldFrom2 0 = Alloc $ return $ Done ()
-- > yieldFrom2 n = let k = yieldFrom2 (n - 1)
-- >                in Alloc $ return $ Yield n k
--
-- This is essentially the same problem as in the non-IO version of 'Source',
-- and this version leaks memory.
{-# NOINLINE yieldFrom2 #-}
yieldFrom2 :: Int -> Source Int ()
yieldFrom2 0 = Alloc $ \w -> (# w, Done () #)
yieldFrom2 n = let k = yieldFrom2 (n - 1)
               in Alloc $ \w -> (# w, Yield n k #)

-- | Version 3
--
-- We can make the dependency explicit by doing this:
--
-- > yieldFrom3 :: Int -> Source Int ()
-- > yieldFrom3 0 = Alloc $ return $ Done ()
-- > yieldFrom3 n = Alloc $ do
-- >                  k <- alloc $ yieldFrom3 (n - 1)
-- >                  return $ Yield n (Alloc (return k))
{-# NOINLINE yieldFrom3 #-}
yieldFrom3 :: Int -> Source Int ()
yieldFrom3 0 = Alloc $ \w0 -> (# w0, Done () #)
yieldFrom3 n = Alloc $ \w0 ->
                 let !(# w1, k #) = alloc (yieldFrom3 (n - 1)) w0
                 in (# w1, Yield n (Alloc (\w' -> (# w', k #))) #)

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

