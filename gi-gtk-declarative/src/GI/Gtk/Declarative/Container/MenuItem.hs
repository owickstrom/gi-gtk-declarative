{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors -fno-warn-orphans #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE RecordWildCards        #-}
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

newSubMenuItem ::
     Gtk.IsWidget w => Text -> IO w -> IO Gtk.Widget
newSubMenuItem label createSubMenu = do
  menuItem' <- Gtk.menuItemNewWithLabel label
  subMenuWidget <- Gtk.unsafeCastTo Gtk.Menu =<< createSubMenu
  Gtk.menuItemSetSubmenu menuItem' (Just subMenuWidget)
  Gtk.toWidget menuItem'

instance Patchable MenuItem where
  create =
    \case
      MenuItem item -> create item
      SubMenu label subMenu' ->
        newSubMenuItem label (create subMenu')
  patch (MenuItem (c1 :: Bin i1 Widget e1)) (MenuItem (c2 :: Bin i2 Widget e2)) =
    case eqT @i1 @i2 of
      Just Refl -> patch c1 c2
      Nothing   -> Replace (create c2)
  patch (SubMenu l1 c1) (SubMenu l2 c2)
    -- TODO: case for l1 /= l2
    | l1 == l2 =
      case patch c1 c2 of
        Modify modify ->
          Modify $ \w -> do
            subMenuItem <- Gtk.unsafeCastTo Gtk.MenuItem w
            Just subMenu' <- Gtk.menuItemGetSubmenu subMenuItem
            modify subMenu'
        Replace newSubMenu ->
          Replace (newSubMenuItem l2 newSubMenu)
        Keep -> Keep

  patch _ b2 = Replace (create b2)

instance EventSource MenuItem where
  subscribe (MenuItem item) widget' cb = subscribe item widget' cb
  subscribe (SubMenu _ children) widget' cb = do
    subMenuItem <- Gtk.unsafeCastTo Gtk.MenuItem widget'
    Just subMenu' <- Gtk.menuItemGetSubmenu subMenuItem
    subscribe children subMenu' cb

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
