{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UnliftedFFITypes     #-}

module Data.Dup.Internal (
    dup#
  ) where

import GHC.Prim
import GHC.Exts

foreign import prim "dupClosure" dupClosure# ::
  forall s. Any -> State# s -> (# State# s, Any #)

{-# NOINLINE dup# #-}
dup# :: forall a s. a -> State# s -> (# State# s, a #)
dup# a s0 =
    case dupClosure# (unsafeCoerce# a) s0
      of (# s1, x #) -> (# s1, unsafeCoerce# x #)
