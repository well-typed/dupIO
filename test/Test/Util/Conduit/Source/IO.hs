module Test.Util.Conduit.Source.IO (
    Source(..)
  , Step(..)
  , yieldFrom1
  , yieldFrom2
  , yieldFrom3
  ) where

import Prelude hiding (IO)

import Test.Util.TestSetup (IO)

-- | IO-based conduit
--
-- One might think that we can avoid the sharing problem by baking IO right into
-- the definition of the conduit itself. While this is true in theory, it means
-- that each and every conduit must be defined very carefully, and the most
-- "natural" way to define many conduits is not correct. This is illustrated in
-- the definitions of 'yieldFrom1', 'yieldFrom2' and 'yieldFrom3'.
newtype Source b r = Alloc { alloc :: IO (Step b r) }

data Step b r =
    Done r
  | Yield b (Source b r)

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

