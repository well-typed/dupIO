module Test.Util.Conduit.Source.Bidirectional (
    Source(..)
  ) where

data Source b' b r =
    Done r
  | Yield b (b' -> Source b' b r)

