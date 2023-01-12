module Test.Util.Conduit.Bidirectional (
    -- * Definition
    Conduit(..)
    -- * Auxiliary
  , Sink
  , Source
  , Closed
    -- * Auxiliary
  , NoEffects
  , noYields
  , noAwaits
  , noEffects
  ) where

import Data.Kind
import Data.Void

{-------------------------------------------------------------------------------
  Conduit definition
-------------------------------------------------------------------------------}

-- | Some conduit flavour
data Conduit a' a b' b m r =
    Await a' (a  -> Conduit a' a b' b m r )
  | Yield b  (b' -> Conduit a' a b' b m r )
  | Lift     (m    (Conduit a' a b' b m r))
  | Done    r

{-------------------------------------------------------------------------------
  Derived
-------------------------------------------------------------------------------}

type Sink   a   m r = Conduit ()   a  () Void m r -- ^ Only awaits
type Source   b m r = Conduit Void () () b    m r -- ^ Only yields
type Closed     m r = Conduit Void () () Void m r -- ^ Neither awaits nor yields

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Lifted version of 'Void'
data NoEffects (a :: Type)

noYields :: Void -> a
noYields x = case x of {}

noAwaits :: Void -> a
noAwaits x = case x of {}

noEffects :: NoEffects a -> b
noEffects x = case x of {}