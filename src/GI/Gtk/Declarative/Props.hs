{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Property lists on declarative objects, supporting the underlying
-- attributes from "Data.GI.Base.Attributes".

module GI.Gtk.Declarative.Props
  ( PropPair(..)
  , classes
  , on
  ) where

import qualified Data.GI.Base.Attributes as GI
import qualified Data.GI.Base.Signals    as GI
import qualified Data.HashSet            as HashSet
import           Data.Text               (Text)
import           Data.Typeable
import           GHC.TypeLits            (KnownSymbol, Symbol)
import qualified GI.Gtk                  as Gtk

import           GI.Gtk.Declarative.CSS

infix 5 :=

type family PureSignalCallback t e where
  PureSignalCallback (IO ()) e = e
  PureSignalCallback (a -> b) e = a -> PureSignalCallback b e

data PropPair w where
  (:=)
    :: (GI.AttrOpAllowed 'GI.AttrConstruct info w
      , GI.AttrOpAllowed 'GI.AttrSet info w
      , GI.AttrGetC info w attr getValue
      , GI.AttrSetTypeConstraint info setValue
      , KnownSymbol attr
      , Typeable attr
      , Eq getValue
      )
    =>  GI.AttrLabelProxy (attr :: Symbol) -> setValue -> PropPair w
  Classes
    :: Gtk.IsWidget w
    => ClassSet
    -> PropPair w
  OnSignal
    :: (Gtk.GObject w, GI.SignalInfo info)
    => Gtk.SignalProxy w info
    -> (w -> GI.HaskellCallbackType info)
    -> PropPair w

classes :: Gtk.IsWidget w => [Text] -> PropPair w
classes = Classes . HashSet.fromList

on :: (Gtk.GObject w, GI.SignalInfo info)
    => Gtk.SignalProxy w info
    -> (w -> GI.HaskellCallbackType info)
    -> PropPair w
on = OnSignal
