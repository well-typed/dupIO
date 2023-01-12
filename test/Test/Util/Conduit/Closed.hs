module Test.Util.Conduit.Closed (
    Closed(..)
  ) where

data Closed m r =
    Done r
  | Lift (m (Closed m r))