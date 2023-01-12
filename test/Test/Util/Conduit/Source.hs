module Test.Util.Conduit.Source (
    Source(..)
  ) where

data Source b r =
    Done r
  | Yield b (Source b r)
