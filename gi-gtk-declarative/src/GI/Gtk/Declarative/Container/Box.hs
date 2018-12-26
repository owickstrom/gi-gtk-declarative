{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors -fno-warn-orphans #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Implementation of 'Gtk.Box' as a declarative container.
module GI.Gtk.Declarative.Container.Box
  ( BoxChild (..)
  , BoxChildProperties (..)
  , defaultBoxChildProperties
  )
where

import           Data.Word                          (Word32)
import qualified GI.Gtk                             as Gtk

import           GI.Gtk.Declarative.Container.Class
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Widget

-- | Describes a child widget to be added with 'boxAppend' to a 'Box'.
data BoxChild event = BoxChild
  { properties :: BoxChildProperties
  , child      :: Widget event
  }
  deriving (Functor)

data BoxChildProperties = BoxChildProperties
  { expand  :: Bool
  , fill    :: Bool
  , padding :: Word32
  }

defaultBoxChildProperties :: BoxChildProperties
defaultBoxChildProperties =
  BoxChildProperties {expand = False, fill = False, padding = 0}

instance Patchable BoxChild where
  create = create . child
  patch s b1 b2 = patch s (child b1) (child b2)

instance EventSource BoxChild where
  subscribe BoxChild{..} = subscribe child

instance ToChildren Gtk.Box [] BoxChild
