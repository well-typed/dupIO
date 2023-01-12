module Test.Util.Conduit.Sink (
    Sink(..)
  ) where

data Sink a r =
    Done r
  | Await (a -> Sink a r)

