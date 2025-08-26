{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -O0 -fno-full-laziness -fno-state-hack #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-stg-final -dsuppress-all -ddump-to-file #-}

{-------------------------------------------------------------------------------
  Experiment with observing GC versus dupIO

  Relevant links

  - https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/gc
  - https://wiki.haskell.org/GHC/Memory_Management
  - http://simonmar.github.io/bib/papers/parallel-gc.pdf
  - http://simonmar.github.io/bib/papers/multicore-ghc.pdf
-------------------------------------------------------------------------------}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import System.Environment
import System.IO.Unsafe
import System.Mem

import Data.Dup

main :: IO ()
main = do
    [experiment] <- getArgs
    case experiment of
      --
      -- Basic demo
      --

      -- If we end on a major GC, we always collect:
      "basic-mM"  -> basicDemo (nop   >> minor) major -- collected
      "basic-MM"  -> basicDemo (nop   >> major) major -- collected
      "basic-mmM" -> basicDemo (minor >> minor) major -- collected
      "basic-mMM" -> basicDemo (minor >> major) major -- collected
      "basic-MmM" -> basicDemo (major >> minor) major -- collected
      "basic-MMM" -> basicDemo (major >> major) major -- collected

      -- If the object is only promoted once, we also always collect:
      "basic-mm"  -> basicDemo (nop   >> minor) minor -- collected
      "basic-Mm"  -> basicDemo (nop   >> major) minor -- collected

      -- However, if the object is promoted /twice/, then a minor GC cannot
      -- collect it.
      "basic-mmm" -> basicDemo (minor >> minor) minor -- collected
      "basic-mMm" -> basicDemo (minor >> major) minor -- NOT collected
      "basic-Mmm" -> basicDemo (major >> minor) minor -- NOT collected
      "basic-MMm" -> basicDemo (major >> major) minor -- NOT collected

      -- Not sure why the "mmm" case above is an exception. Why did it not get
      -- promoted? /Three/ minor GCs /do/ seem to promote it to gen1:
      "basic-mmmm" -> basicDemo (minor >> minor >> minor) minor -- NOT collected
      "basic-mmmM" -> basicDemo (minor >> minor >> minor) major -- collected

      --
      -- Key experiments
      --

      "noDupIO-05" -> noDupIO 05
      "noDupIO-20" -> noDupIO 20

      "withDupIO-05" -> withDupIO 05
      "withDupIO-20" -> withDupIO 20

      --
      -- Done
      --

      _otherwise -> putStrLn "Unknown experiment"

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

minor :: IO ()
minor = do performMinorGC ; yield

major :: IO ()
major = do performMajorGC ; yield

nop :: IO ()
nop = yield

data Link = Nil | Link_  (IORef ()) Int Link

pattern Link :: Int -> Link -> Link
pattern Link i next <- Link_ _ i next
 where
   Link i next = unsafePerformIO $ do
      ref   <- newIORef ()
      _weak <- mkWeakIORef ref $ putStrLn $ "Collected " ++ show i
      return $ Link_ ref i next

chain :: Int -> Int -> Link
chain i j
  | i > j     = Nil
  | otherwise = Link i $ chain (succ i) j

touch1 :: Link -> IO ()
touch1 = void . touchN 1

touchN :: Int -> Link -> IO Link
touchN 0 l   = return l
touchN _ Nil = return Nil
touchN n (Link_ ref i next) = do
    () <- readIORef ref
    print i
    touchN (pred n) next

{-------------------------------------------------------------------------------
  Basic demo
-------------------------------------------------------------------------------}

basicDemo :: IO () -> IO () -> IO ()
basicDemo firstGC secondGC = do
    putStrLn "basicDemo"
    putStrLn "***"

    localRoot <- evaluate $ Link 0 Nil
    putStrLn "First GC"
    firstGC
    touch1 localRoot

    putStrLn "***"

    putStrLn "Second GC"
    secondGC

    putStrLn "***"
    putStrLn "Exit"

{-------------------------------------------------------------------------------
  Without dupIO
-------------------------------------------------------------------------------}

noDupIO :: Int -> IO ()
noDupIO len = do
    putStrLn "noDupIO"
    putStrLn "***"

    let left  = chain 100 (100 + len - 1)
        right = chain 200 (200 + len - 1)

    let process :: Link -> IO ()
        process Nil = return ()
        process l   = do
            l' <- touchN 5 l
            minor
            process l'

    process left
    process right

    putStrLn "***"

{-------------------------------------------------------------------------------
  With dupIO

  NOTE: It is critical for a clear demo that withDupIO has an argument (/any/
  argument, could be @()@); if not, `withDupIO` would itself become a CAF which
  would retain `left` and `right` which would muddy the GC waters.
-------------------------------------------------------------------------------}

withDupIO :: Int -> IO ()
withDupIO len = do
    putStrLn "noDupIO"
    putStrLn "***"

    let left  = chain 100 (100 + len - 1)
        right = chain 200 (200 + len - 1)

    let process :: Link -> IO ()
        process Nil = return ()
        process l   = do
            l' <- touchN 5 l
            minor
            process l'

    process =<< dupIO left
    process =<< dupIO right

    putStrLn "***"









