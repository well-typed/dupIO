{-# LANGUAGE MagicHash #-}

module Data.Dup.ST (
    dupST
  ) where

import GHC.ST

import Data.Dup.Internal

-- | 'ST' version of 'Data.Dup.IO.dupIO'
--
-- See 'Data.Dup.IO.dupIO' for detailed discussion.
dupST :: a -> ST s a
dupST = ST . dup#

