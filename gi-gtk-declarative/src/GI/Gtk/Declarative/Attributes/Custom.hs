{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module GI.Gtk.Declarative.Attributes.Custom
  ( CustomAttribute (..)
  , CustomAttributeDecl (..)
  , CustomAttributeState (..)
  , DeclWrap (..)
  , StateWrap (..)
  ) where

import Data.Typeable

import GI.Gtk.Declarative.EventSource.Subscription

-- | The declarative form of a custom attribute.
data CustomAttributeDecl widget event where
  CustomAttributeDecl
    :: CustomAttribute widget decl state
    => DeclWrap decl state event
    -> CustomAttributeDecl widget event

instance Functor (CustomAttributeDecl widget) where
  fmap f (CustomAttributeDecl (DeclWrap attr)) = CustomAttributeDecl (fmap f (DeclWrap attr))

-- | The runtime state that is maintained for a custom attribute.
data CustomAttributeState widget where
  CustomAttributeState
    :: CustomAttribute widget decl state
    => StateWrap decl state
    -> CustomAttributeState widget

-- These are needed to get GHC to figure out the right types.
-- It would be good to get rid of them somehow
data StateWrap (decl :: * -> *) state = StateWrap state
data DeclWrap (decl :: * -> *) state event = DeclWrap (decl event)
  deriving (Functor)

-- todo: AttrPatch data type

class (Typeable decl, Typeable state, Functor decl) => CustomAttribute widget decl state | decl -> state, state -> decl where
  attrCreate :: widget -> decl event -> IO state
  attrPatch :: widget -> state -> decl event1 -> decl event2 -> IO state
  attrDestroy :: widget -> state -> decl event -> IO ()
  attrSubscribe :: widget -> state -> decl event -> (event -> IO ()) -> IO Subscription
