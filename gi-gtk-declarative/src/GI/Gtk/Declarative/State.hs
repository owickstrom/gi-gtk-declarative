{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
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
       , Typeable customState
       )
    => StateTree stateType widget child event customState
    -> SomeState

data StateType = WidgetState | BinState | ContainerState

data StateTree (stateType :: StateType) widget child event customState where
  StateTreeWidget
    :: !(StateTreeNode widget event customState)
    -> StateTree 'WidgetState widget child event customState
  StateTreeBin
    :: !(StateTreeNode widget event customState)
    -> SomeState
    -> StateTree 'BinState widget child event customState
  StateTreeContainer
    :: ( Gtk.IsContainer widget
       , IsContainer widget child
       )
    => !(StateTreeNode widget event customState)
    -> Vector SomeState
    -> StateTree 'ContainerState widget child event customState

data StateTreeNode widget event customState = StateTreeNode
  { stateTreeWidget              :: !widget
  , stateTreeStyleContext        :: !Gtk.StyleContext
  , stateTreeCollectedAttributes :: !(Collected widget event)
  , stateTreeCustomState         :: customState
  }

stateTreeNode
  :: StateTree stateType widget child event customState
  -> StateTreeNode widget event customState
stateTreeNode (StateTreeWidget s     ) = s
stateTreeNode (StateTreeBin       s _) = s
stateTreeNode (StateTreeContainer s _) = s

stateTreeNodeWidget :: StateTree stateType widget child event customState -> widget
stateTreeNodeWidget = stateTreeWidget . stateTreeNode

someStateWidget :: SomeState -> IO Gtk.Widget
someStateWidget (SomeState st) = Gtk.toWidget (stateTreeNodeWidget st)
