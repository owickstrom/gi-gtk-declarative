{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
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

-- | Implementation of 'Gtk.Paned' as a declarative container.
module GI.Gtk.Declarative.Container.Paned
  ( Pane (Pane, resize, shrink)
  , Resize(..)
  , Shrink(..)
  , pane
  )
where

import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch

-- | Describes a pane to be packed with
-- 'Gtk.panePack1'/'Gtk.panePack2' in a 'Gtk.Paned'.
data Pane event = Pane
  { resize    :: Resize
  , shrink    :: Shrink
  , paneChild :: Widget event
  }
  deriving (Functor)

newtype Resize = Resize Bool
newtype Shrink = Shrink Bool

-- | Construct a pane to be packed with
-- 'Gtk.panePack1'/'Gtk.panePack2' in a 'Gtk.Paned'.
pane :: Resize -> Shrink -> Widget event -> MarkupOf Pane event ()
pane resize shrink paneChild = single Pane {..}

instance Patchable Pane where
  create = create . paneChild
  patch s b1 b2 = patch s (paneChild b1) (paneChild b2)

instance EventSource Pane where
  subscribe Pane{..} = subscribe paneChild
