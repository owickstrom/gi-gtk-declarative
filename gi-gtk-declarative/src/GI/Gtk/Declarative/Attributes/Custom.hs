{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}

module GI.Gtk.Declarative.Attributes.Custom
  ( CustomAttribute (..)
  , CustomAttributeDecl (..)
  , CustomAttributeState (..)
  ) where

import           Data.Typeable

import           GI.Gtk.Declarative.EventSource.Subscription

-- | The declarative form of a custom attribute.
data CustomAttributeDecl widget event where
  CustomAttributeDecl
    :: CustomAttribute widget decl
    => decl event
    -> CustomAttributeDecl widget event

deriving instance Functor (CustomAttributeDecl widget)

-- | The runtime state that is maintained for a custom attribute.
data CustomAttributeState widget where
  CustomAttributeState
    :: CustomAttribute widget decl
    => State decl
    -> CustomAttributeState widget

-- todo: AttrPatch data type

class (Typeable decl, Typeable (State decl), Functor decl) => CustomAttribute widget decl where
  data State decl
  attrCreate :: widget -> decl event -> IO (State decl)
  attrPatch :: widget -> State decl -> decl event1 -> decl event2 -> IO (State decl)
  attrDestroy :: widget -> State decl -> decl event -> IO ()
  attrSubscribe :: widget -> State decl -> decl event -> (event -> IO ()) -> IO Subscription
