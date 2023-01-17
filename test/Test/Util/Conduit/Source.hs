module Test.Util.Conduit.Source (
    Source(..)
  , yieldFrom
  ) where

data Source b r =
    Done r
  | Yield b (Source b r)

{-# NOINLINE yieldFrom #-}
yieldFrom :: Int -> Source Int ()
yieldFrom 0 = Done ()
yieldFrom n = let k = yieldFrom (n - 1)
              in Yield n $ k

