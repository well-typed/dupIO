module Test.Util.Conduit.Sink (
    Sink(..)
  , Box(..)
  ) where

data Box a = Box { unbox :: a }

data Sink a r =
    Done r
  | Await (a -> Box (Sink a r))

