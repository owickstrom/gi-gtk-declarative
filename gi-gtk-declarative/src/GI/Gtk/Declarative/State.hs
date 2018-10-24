{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
module GI.Gtk.Declarative.State where

import           Data.Typeable

import           Data.Vector                             (Vector)
import qualified GI.Gtk                                  as Gtk

import           GI.Gtk.Declarative.Attributes.Collected
import           GI.Gtk.Declarative.Container.Class

data SomeState where
  SomeState
    :: ( Gtk.IsWidget widget
       , Typeable widget
       )
    => StateTree stateType widget child event
    -> SomeState

data StateType = WidgetState | BinState | ContainerState

data StateTree (stateType :: StateType) widget child event where
  StateTreeWidget
    :: !(StateTreeNode widget event)
    -> StateTree 'WidgetState widget child event
  StateTreeBin
    :: !(StateTreeNode widget event)
    -> SomeState
    -> StateTree 'BinState widget child event
  StateTreeContainer
    :: ( Gtk.IsContainer widget
       , IsContainer widget child
       )
    => !(StateTreeNode widget event)
    -> Vector SomeState
    -> StateTree 'ContainerState widget child event

data StateTreeNode widget event = StateTreeNode
  { stateTreeWidget              :: !widget
  , stateTreeStyleContext        :: !Gtk.StyleContext
  , stateTreeCollectedAttributes :: !(Collected widget event)
  }

stateTreeNode
  :: StateTree stateType widget child event -> StateTreeNode widget event
stateTreeNode (StateTreeWidget s     ) = s
stateTreeNode (StateTreeBin       s _) = s
stateTreeNode (StateTreeContainer s _) = s

stateTreeNodeWidget :: StateTree stateType widget child event -> widget
stateTreeNodeWidget = stateTreeWidget . stateTreeNode

someStateWidget :: SomeState -> IO Gtk.Widget
someStateWidget (SomeState st) = Gtk.toWidget (stateTreeNodeWidget st)
