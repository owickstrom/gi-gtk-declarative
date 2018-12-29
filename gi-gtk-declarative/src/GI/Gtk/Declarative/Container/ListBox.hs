{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GI.Gtk.Declarative.Container.ListBox where

import           Data.Vector                        (Vector)
import qualified GI.Gtk                             as Gtk

import           GI.Gtk.Declarative.Bin
import           GI.Gtk.Declarative.Container.Class
import           GI.Gtk.Declarative.Widget

instance ToChildren Gtk.ListBox Vector (Bin Gtk.ListBoxRow Widget)
