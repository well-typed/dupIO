module Test.Util.Conduit.Source.IO (
    Source(..)
  , Step(..)
  ) where

import Prelude hiding (IO)

import Test.Util.TestSetup (IO)

newtype Source b r = Alloc { alloc :: IO (Step b r) }

data Step b r =
    Done r
  | Yield b (Source b r)

