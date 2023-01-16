module Test.Util.Conduit.Closed (
    Closed(..)
  , addFrom
  ) where

import Data.IORef

data Closed m r =
    Done r
  | Lift (m (Closed m r))

{-# NOINLINE addFrom #-}
addFrom :: IORef Int -> Int -> Closed Prelude.IO ()
addFrom ref = go
  where
    go :: Int -> Closed Prelude.IO ()
    go 0 = Done ()
    go n = Lift $ modifyIORef' ref (+ n) >> return (go (n - 1))

