{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors -fno-warn-orphans #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module GI.Gtk.Declarative.Container.MenuItem
  ( MenuItem
  , menuItem
  , subMenu
  )
where

import           Data.Text                      ( Text )
import           Data.Typeable
import           Data.Vector                    ( Vector )
import qualified GI.Gtk                        as Gtk

import           GI.Gtk.Declarative.Attributes
import           GI.Gtk.Declarative.Bin
import           GI.Gtk.Declarative.Container
import           GI.Gtk.Declarative.Container.Patch
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.State
import           GI.Gtk.Declarative.Widget

-- | A menu item widget used for 'Gtk.Menu' children.
data MenuItem event where
  -- | A single menu item in a 'Gtk.Menu'.
  MenuItem
    ::(Gtk.IsMenuItem item, Gtk.IsBin item, Typeable item)
    => Bin item event
    -> MenuItem event
  -- | A sub menu in a 'Gtk.Menu', with a text label and the list of
  -- child menu items.
  SubMenu
    ::Text -> Container Gtk.Menu (Children MenuItem) event -> MenuItem event

instance Functor MenuItem where
  fmap f (MenuItem item         ) = MenuItem (fmap f item)
  fmap f (SubMenu label subMenu') = SubMenu label (fmap f subMenu')

instance ToChildren Gtk.Menu Vector MenuItem

instance ToChildren Gtk.MenuBar Vector MenuItem

-- | Construct a single menu item for a 'Gtk.Menu'.
menuItem
  :: ( Gtk.IsMenuItem item
     , Typeable item
     , Gtk.IsContainer item
     , Gtk.IsBin item
     , Gtk.IsWidget item
     )
  => (Gtk.ManagedPtr item -> item)
  -> Vector (Attribute item event)
  -> Widget event
  -> MenuItem event
menuItem item attrs = MenuItem . Bin item attrs

-- | Construct a sub menu for a 'Gtk.Menu', wit a text label and the
-- child menu items.
subMenu :: Text -> Vector (MenuItem event) -> MenuItem event
subMenu label = SubMenu label . container Gtk.Menu mempty

newSubMenuItem :: Text -> IO SomeState -> IO SomeState
newSubMenuItem label createSubMenu = do
  menuItem' <- Gtk.menuItemNewWithLabel label
  sc        <- Gtk.widgetGetStyleContext menuItem'
  SomeState (subMenuState :: StateTree st subMenu children e1 cs) <-
    createSubMenu
  case eqT @subMenu @Gtk.Menu of
    Just Refl -> do
      Gtk.menuItemSetSubmenu menuItem' (Just (stateTreeNodeWidget subMenuState))
      return
        (SomeState
          (StateTreeBin (StateTreeNode menuItem' sc mempty ())
                        (SomeState subMenuState)
          )
        )
    Nothing -> fail "Failed to create new sub menu item."

instance Patchable MenuItem where
  create = \case
    MenuItem item          -> create item
    SubMenu label subMenu' -> newSubMenuItem label (create subMenu')
  patch state (MenuItem (c1 :: Bin i1 e1)) (MenuItem (c2 :: Bin i2 e2)) =
    case eqT @i1 @i2 of
      Just Refl -> patch state c1 c2
      Nothing   -> Replace (create c2)
  patch (SomeState st) (SubMenu l1 c1) (SubMenu l2 c2) = case st of
    StateTreeBin top childState | l1 == l2 -> case patch childState c1 c2 of
      Modify  modify     -> Modify (SomeState . StateTreeBin top <$> modify)
      Replace newSubMenu -> Replace (newSubMenuItem l2 newSubMenu)
      Keep               -> Keep
    -- TODO: case for l1 /= l2
    _ -> Replace (create (SubMenu l2 c2))
  patch _ _ b2 = Replace (create b2)

instance EventSource MenuItem where
  subscribe (MenuItem item     ) state          cb = subscribe item state cb
  subscribe (SubMenu _ children) (SomeState st) cb = case st of
    StateTreeBin _ childState -> subscribe children childState cb
    _                         -> error
      "Warning: Cannot subscribe to SubMenu events with a non-bin state tree."

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
