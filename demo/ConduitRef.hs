{-# OPTIONS_GHC -O1 #-}

-- | Version of @demo/Conduit.hs@ written in a more conventional style
--
-- This depends on @-O1@, for reasons explained in @demo/Conduit.hs@.
module Main (main) where

import Data.Dup (dup)

data Source = Done | Yield {-# UNPACK #-} !Int Source

yieldFrom :: Int -> Source
yieldFrom 0 = Done
yieldFrom n = Yield n (yieldFrom (n - 1))

runSource :: Source -> IO ()
runSource = go
  where
    go :: Source -> IO ()
    go src =
        case dup src of
          Done      -> return ()
          Yield n k -> print n >> go k

twice :: IO () -> IO ()
twice io = io >> io

-- | Main application
--
-- Suggested invocation:
--
-- > cabal run demo-conduit-ref -- +RTS -s >log
main :: IO ()
main = twice $ runSource $ yieldFrom 1_000_000