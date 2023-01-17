{-# LANGUAGE MagicHash #-}

module Data.Dup (
    dupIO
  , dupST
  , dup
  ) where

import GHC.IO
import GHC.ST

import Data.Dup.Internal

-- | Make shallow copy of a closure
--
-- = Motivation
--
-- In Haskell we often use data structures that do not represent /data/ per se
-- but rather /control information/ for our code. A typical example is a conduit
-- (<https://hackage.haskell.org/package/conduit>) or a pipe
-- (<https://hackage.haskell.org/package/pipes>); these are monadic abstractions
-- intended to model streaming computations: a conduit can /await/ a value
-- coming in, /yield/ a value going out, or terminate and /return/ a result.
-- Consider this extremely simplified form of a conduit, which only ever yields
-- integers (a conduit that only yields values is also known as a /source/):
--
-- > data Source = Done | Yield {-# UNPACK #-} !Int Source
--
-- For example, here is a source which yields the values from @n@ to @0@:
--
-- > yieldFrom :: Int -> Source
-- > yieldFrom 0 = Done
-- > yieldFrom n = Yield n $ yieldFrom (n - 1)
--
-- We can define many interpreters over such a source; here is one that simply
-- prints all yielded values:
--
-- > runSource :: Source -> IO ()
-- > runSource = go
-- >   where
-- >     go :: Source -> IO ()
-- >     go src =
-- >         case src of
-- >           Done      -> return ()
-- >           Yield n k -> print n >> go k
--
-- Unfortunately, this code has a problem. Suppose this is our main application:
--
-- > twice :: IO () -> IO ()
-- > twice io = io >> io
-- >
-- > main :: IO ()
-- > main = twice $ runSource $ yieldFrom 1_000_000
--
-- The problem with this application is that it unrolls the entire @yieldFrom@
-- conduit in memory the first time @runSource@ executes: garbage collection
-- cannot clean up after it, because there is still a reference to the same
-- conduit, since @twice@ will execute it /again/. While this is obviously a
-- contrived example, such problems do arise in real applications and can be
-- fiendishly difficult to debug; see the blog post "Sharing, Space Leaks, and
-- Conduit and friends" by Edsko de Vries
-- (<https://well-typed.com/blog/2016/09/sharing-conduit/>) for a detailed
-- discussion.
--
-- The problem is ironic, because abstractions such as conduits are intended to
-- model applications that can stream data with constant memory usage; yet, here
-- it is the conduit /itself/ that is taking up a lot of memory. We really don't
-- want to store such control data in memory, ever: we want to recompute it
-- whenever necessary. Writing the code in such a way that we avoid sharing of
-- conduits is extremely challenging, but fortunately we can solve the problem
-- in another way. We can change @runSource@ to
--
-- > runSource :: Source -> IO ()
-- > runSource = go
-- >   where
-- >     go :: Source -> IO ()
-- >     go src = do
-- >         src' <- dupIO src
-- >         case src' of
-- >           Done      -> return ()
-- >           Yield n k -> print n >> go k
--
-- By duplicating the closure before we pattern match on it, the original
-- closure remains a thunk, and we never unfold the conduit in memory.
--
-- = Use in pure contexts
--
-- The disadvantage of 'dupIO' is that it lives in @IO@. It /is/ possible to
-- rewrite 'runSource' using the pure function @dup@ as:
--
-- > runSource :: Source -> IO ()
-- > runSource = go
-- >   where
-- >     go :: Source -> IO ()
-- >     go src = do
-- >         case dup src of
-- >           Done      -> return ()
-- >           Yield n k -> print n >> go k
--
-- This then opens the way to interpreters that don't live in @IO@. Be aware
-- however that in some cases GHC might be able to float @(dup ..)@ expressions
-- out, re-introducing some unwanted sharing. Where possible, 'dupIO' or 'dupST'
-- is probably the safer choice. In examples like this, however, 'dup' is
-- (probably) fine, because there is only so much floating out that ghc can do
-- here.
--
-- __CAUTION__. It is perhaps tempting to think that we could introduce 'dup'
-- in the constructors of the conduit instead, for example introducing a smart
-- constructor such as
--
-- > yield :: Int -> Source -> Source
-- > yield n k = dup $ Yield n k -- NOT OK
--
-- This however does not work. While it is true that /the first time/ we pattern
-- match on this source we will work with a copy, the /second time/ the 'dup'
-- will have disappeared and we are back to square 1. We have to make sure that
-- we duplicate the closure /every time/ we pattern match on it.
--
-- = Relation to @unsafePerformIO@ and @unsafeInterleaveIO@ (lazy I\/O)
--
-- When a closure is defined with @unsafePerformIO@ and is subsequently
-- duplicated, the effect might be duplicated too. Consider this example:
--
-- > ref :: IORef Int <- newIORef 0
-- >
-- > let thunk :: ()
-- >     thunk = unsafePerformIO $ modifyIORef ref (+ 1)
-- >
-- > evaluate =<< dupIO thunk
-- > evaluate =<< dupIO thunk
--
-- This will update the @IORef@ /twice/. The same happens if the thunk is
-- defined using @unsafeInterleaveIO@:
--
-- > ..
-- > thunk :: () <- unsafeInterleaveIO $ modifyIORef ref (+ 1)
-- > ..
--
-- Lazy I\/O (such as 'hGetContents') uses 'unsafeInterleaveIO'; combining lazy
-- I\/O and 'dupIO' is almost certainly a recipe for disaster.
dupIO :: a -> IO a
dupIO = IO . dup#

-- | Pure version of 'dupIO'
--
-- Be careful with the use of 'dup', as @ghc@ might float expressions containing
-- 'dup' outwards, introducing unwanted sharing. Consider using 'dupIO' or
-- 'dupST' instead.
--
-- See 'dupIO' for detailed discussion.
dup :: a -> a
dup = unsafePerformIO . dupIO

-- | Version of 'dupIO' for the 'ST' monad
--
-- This might be a useful compromise between 'dupIO' and 'dup'. Unlike 'dup',
-- 'dupST' cannot be moved: we can control when the duplication happens. Unlike
-- 'dupIO', functions that use 'dupST' can still be pure on the outside.
--
-- See 'dupIO' for detailed discussion.
dupST :: a -> ST s a
dupST = ST . dup#





