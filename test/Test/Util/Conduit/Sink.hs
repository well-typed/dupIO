module Test.Util.Conduit.Sink (
    Sink(..)
  , Box(..)
  , countChars
  ) where

data Box a = Box { unbox :: a }

data Sink a r =
    Done r
  | Await (a -> Box (Sink a r))

-- | Sink that counts how many characters it is fed
--
-- The strange way that 'countChars' is written is modelling what
-- full laziness may very well do to your conduit.
{-# NOINLINE countChars #-}
countChars :: Int -> Sink (Maybe Char) Int
countChars cnt =
    let k = let !cnt' = cnt + 1
            in countChars cnt'
    in Await $ \case
         Nothing -> Box $ Done cnt
         Just _  -> Box $ k

