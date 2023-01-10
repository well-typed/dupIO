module Data.DupIO (dupIO) where

dupIO :: a -> IO a
dupIO = return