{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Implementation of 'Gtk.Box' as a declarative container.
module GI.Gtk.Declarative.Container.Box
  ( BoxChild(..)
  , BoxChildProperties(..)
  , defaultBoxChildProperties
  )
where

import           Data.Default.Class             ( Default(def) )
import           Data.Vector                    ( Vector )
import           Data.Word                      ( Word32 )
import qualified GI.Gtk                        as Gtk

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

-- | Values used when /packing/ child widgets into boxes.
data BoxChildProperties = BoxChildProperties
  { expand  :: Bool
  , fill    :: Bool
  , padding :: Word32
  } deriving (Eq, Show)

-- | Defaults for 'BoxChildProperties'. Use these and override
-- specific fields.
defaultBoxChildProperties :: BoxChildProperties
defaultBoxChildProperties =
  BoxChildProperties { expand = False, fill = False, padding = 0 }

instance Default BoxChildProperties where
  def = defaultBoxChildProperties

instance Patchable BoxChild where
  create = create . child
  patch s b1 b2 | properties b1 == properties b2 = patch s (child b1) (child b2)
                | otherwise                      = Replace (create b2)

instance EventSource BoxChild where
  subscribe BoxChild {..} = subscribe child

instance ToChildren Gtk.Box Vector BoxChild

instance IsContainer Gtk.Box BoxChild where
  appendChild box BoxChild { properties = BoxChildProperties { expand, fill, padding } } widget'
    = Gtk.boxPackStart box widget' expand fill padding
  replaceChild box boxChild' i old new = do
    Gtk.widgetDestroy old
    appendChild box boxChild' new
    Gtk.boxReorderChild box new i
