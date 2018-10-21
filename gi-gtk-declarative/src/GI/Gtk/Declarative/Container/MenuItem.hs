{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors -fno-warn-orphans #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module GI.Gtk.Declarative.Container.MenuItem
  ( MenuItem
  , menuItem
  , subMenu
  )
where

import           Data.Text                          (Text)
import           Data.Typeable
import qualified GI.Gtk                             as Gtk

import           GI.Gtk.Declarative.Attributes
import           GI.Gtk.Declarative.Bin
import           GI.Gtk.Declarative.Container
import           GI.Gtk.Declarative.Container.Patch
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.State

data MenuItem event where
  MenuItem
    :: (Gtk.IsMenuItem item, BinChild item Widget, Typeable item)
    => Bin item Widget event
    -> MenuItem event
  SubMenu
    :: Text -> (Container Gtk.Menu (Children MenuItem) event) -> MenuItem event

instance Functor MenuItem where
  fmap f (MenuItem item) = MenuItem (fmap f item)
  fmap f (SubMenu label subMenu')= SubMenu label (fmap f subMenu')

menuItem ::
     ( Gtk.IsMenuItem item
     , Typeable event
     , BinChild item Widget
     , Typeable item
     , Gtk.IsContainer item
     , Gtk.IsBin item
     , Gtk.IsWidget item
     )
  => (Gtk.ManagedPtr item -> item)
  -> [Attribute item event]
  -> Widget event
  -> MarkupOf MenuItem event ()
menuItem item attrs = single . MenuItem . Bin item attrs

subMenu ::
     (Typeable event)
  => Text
  -> MarkupOf MenuItem event ()
  -> MarkupOf MenuItem event ()
subMenu label = single . SubMenu label . container Gtk.Menu []

newSubMenuItem :: Text -> IO StateTree -> IO StateTree
newSubMenuItem label createSubMenu = do
  menuItem' <- Gtk.menuItemNewWithLabel label
  sc <- Gtk.widgetGetStyleContext menuItem'
  subMenuState <- createSubMenu
  subMenuWidget <- Gtk.unsafeCastTo Gtk.Menu (stateTreeNodeWidget subMenuState)
  Gtk.menuItemSetSubmenu menuItem' (Just subMenuWidget)
  w <- Gtk.toWidget menuItem'
  return (StateTreeBin (StateTreeNode w sc) subMenuState)

instance Patchable MenuItem where
  create =
    \case
      MenuItem item -> create item
      SubMenu label subMenu' ->
        newSubMenuItem label (create subMenu')
  patch state (MenuItem (c1 :: Bin i1 Widget e1)) (MenuItem (c2 :: Bin i2 Widget e2)) =
    case eqT @i1 @i2 of
      Just Refl -> patch state c1 c2
      Nothing   -> Replace (create c2)
  patch (StateTreeBin top childState) (SubMenu l1 c1) (SubMenu l2 c2)
    -- TODO: case for l1 /= l2
    | l1 == l2 =
      case patch childState c1 c2 of
        Modify modify ->
          Modify (StateTreeBin top <$> modify)
        Replace newSubMenu ->
          Replace (newSubMenuItem l2 newSubMenu)
        Keep -> Keep

  patch _ _ b2 = Replace (create b2)

instance EventSource MenuItem where
  subscribe (MenuItem item) state cb = subscribe item state cb
  subscribe (SubMenu _ children) (StateTreeBin _ childState) cb =
    subscribe children childState cb
  subscribe SubMenu{} _ _ =
    error "Warning: Cannot subscribe to SubMenu events with a non-bin state tree."

instance IsContainer Gtk.MenuShell MenuItem where
  appendChild shell _ widget' =
    Gtk.menuShellAppend shell =<< Gtk.unsafeCastTo Gtk.MenuItem widget'
  replaceChild shell _ i old new = do
    Gtk.containerRemove shell old
    menuItem' <- Gtk.unsafeCastTo Gtk.MenuItem new
    Gtk.menuShellInsert shell menuItem' i
    Gtk.widgetShowAll shell

instance IsContainer Gtk.MenuBar MenuItem where
  appendChild menuBar d w = do
    s <- Gtk.toMenuShell menuBar
    appendChild s d w
  replaceChild menuBar d i old new = do
    s <- Gtk.toMenuShell menuBar
    replaceChild s d i old new

instance IsContainer Gtk.Menu MenuItem where
  appendChild menuBar d w = do
    s <- Gtk.toMenuShell menuBar
    appendChild s d w
  replaceChild menuBar d i old new = do
    s <- Gtk.toMenuShell menuBar
    replaceChild s d i old new
