module Test.Util.Conduit.Source.Bidirectional (
    Source(..)
  , Box(..)
  , yieldFrom
  ) where

data Box a = Box a

data Source b' b r =
    Done r
  | Yield b (b' -> Box (Source b' b r))

{-# NOINLINE yieldFrom #-}
yieldFrom :: Int -> Source () Int ()
yieldFrom 0 = Done ()
yieldFrom n = let k = yieldFrom (n - 1)
              in Yield n $ \() -> Box k



