module Test.Util.Conduit.Source.Bidirectional (
    Source(..)
  , Box(..)
  ) where

data Box a = Box a

data Source b' b r =
    Done r
  | Yield b (b' -> Box (Source b' b r))



