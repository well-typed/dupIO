{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash     #-}

module Data.Dup.IO (
    dupIO
  ) where

import GHC.IO

import Data.Dup.Internal

-- | Duplicate closure
--
-- Not all closures are duplicated; in particular, closures with no pointer
-- fields are returned as-is (duplicating such closures would serve no purpose).
-- However, closures that might be mutated (through evaluation) /will/ be
-- duplicated, the most important example being thunks.
--
-- = Using 'dupIO' to control sharing
--
-- The textbook example of unwanted sharing is the @average@ function:
--
-- > average :: [Int] -> Int
-- > average xs = foldl' (+) 0 xs `div` length xs
--
-- If we now compute @average [1 .. 10_000_000]@, the computation of the sum
-- of the elements of the list will build the entire list in memory: since we
-- still need to compute the length of the list as a second step, the garbage
-- collector cannot clean up as we go.
--
-- One solution to this problem is to compute the sum and the length together,
-- rather than as two separate computations (for example, using the
-- <https://hackage.haskell.org/package/foldl> library). With 'dupIO' we have
-- another solution:
--
-- > average :: [Int] -> IO Int
-- > average xs = do
-- >     xs' <- dupIO xs
-- >     return $ foldl' (+) 0 xs `div` length xs'
--
-- By duplicating the thunk, the sum and the length are computed independently
-- from each other, without ever building the list in memory. The copy itself
-- is cheap, as it only makes a shallow copy. The downside is that if the list
-- is expensive to compute, then that computation will happen twice.
--
-- = Usage in pure code
--
-- Although our modified version of @average@ above lives in @IO@, this is
-- easily avoided by using 'Data.Dup.ST.dupST' instead:
--
-- > average :: [Int] -> Int
-- > average xs = runST $ do
-- >     xs' <- dupST xs
-- >     return $ foldl' (+) 0 xs `div` length xs'
--
-- While it is /possible/ to recover the entirely pure interface that the old
-- <https://hackage.haskell.org/package/ghc-dup> package offered, but doing
-- this is __NOT RECOMMENDED__:
--
-- > data Box a = Box a
-- >
-- > dup :: a -> Box a
-- > dup x = unsafePerformIO $ Box <$> dupIO x
--
-- The problem with the pure interface is that you have no control over any @dup
-- xyz@ term; ghc might move it around, and might reintroduce sharing in ways
-- that are quite unpredictable, especially in the presence of full laziness and
-- the state hack (both of which are enabled in ghc by default). Better to be
-- precise about when closure duplication happens by using the @IO@ or @ST@
-- monad.
--
-- = Observing evaluation
--
-- Although this is not its intended use case, the effect of 'dupIO' is most
-- clearly visible together with @unsafePerformIO@. Consider this example:
--
-- > ref :: IORef Int <- newIORef 0
-- >
-- > let thunk :: ()
-- >     thunk = unsafePerformIO $ modifyIORef ref (+ 1)
-- >
-- > evaluate thunk
-- > evaluate thunk
--
-- In this example we create a thunk which, when evaluated, happens to update an
-- @IORef@. Since Haskell is lazy, however, this will only happen once: when the
-- thunk is evaluated to WHNF the first time, the in-memory representation of
-- the thunk is updated (in this case with the value @()@), and it won't be
-- evaluated again. Using 'dupIO' we can evaluate it as often as we like; if
-- instead of the above, we do
--
-- > evaluate =<< dupIO thunk
-- > evaluate =<< dupIO thunk
--
-- the @IORef@ will be updated twice.
dupIO :: a -> IO a
dupIO = IO . dup#
