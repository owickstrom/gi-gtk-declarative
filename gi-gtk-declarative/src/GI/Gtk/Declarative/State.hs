{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
-- | The 'StateTree' and 'SomeState' form a "shadow state"
-- representation, used in patching. Declarative widgets can return,
-- and later on reuse, its underlying GTK+ widget, collected
-- properties and classes, style context, custom internal state, and
-- child states. This reduces the need for querying GTK+ widgets
-- excessively, and recalculating/resetting, greatly improving the
-- performance of patching.
module GI.Gtk.Declarative.State where

import           Data.Typeable

import           Data.Vector                             (Vector)
import qualified GI.Gtk                                  as Gtk

import           GI.Gtk.Declarative.Attributes.Collected
import           GI.Gtk.Declarative.Container.Class

-- | A 'Data.Dynamic.Dynamic'-like container of a 'StateTree' value.
data SomeState where
  SomeState
    :: ( Gtk.IsWidget widget
      , Typeable widget
      , Typeable customState
      )
    => StateTree stateType widget child event customState
    -> SomeState

-- | The types of state trees that are available, matching the types
-- of GTK+ widgets (single widget, bin, and container.)
data StateType = WidgetState | BinState | ContainerState

-- | A state tree for a specific 'widget'. This is built up recursively
-- to contain child state trees, for bin and container child widgets.
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

-- | The common structure for all state tree nodes.
data StateTreeNode widget event customState = StateTreeNode
  { stateTreeWidget              :: !widget
  , stateTreeStyleContext        :: !Gtk.StyleContext
  , stateTreeCollectedAttributes :: !(Collected widget event)
  , stateTreeCustomState         :: customState
  }

-- * Convenience accessor functions

-- | Get the common state tree node information.
stateTreeNode
  :: StateTree stateType widget child event customState
  -> StateTreeNode widget event customState
stateTreeNode (StateTreeWidget s     ) = s
stateTreeNode (StateTreeBin       s _) = s
stateTreeNode (StateTreeContainer s _) = s

-- | Get the specific type of GTK+ widget of a state tree.
stateTreeNodeWidget :: StateTree stateType widget child event customState -> widget
stateTreeNodeWidget = stateTreeWidget . stateTreeNode

-- | Get the GTK+ widget, cast to 'Gtk.Widget', of /some/ state tree.
someStateWidget :: SomeState -> IO Gtk.Widget
someStateWidget (SomeState st) = Gtk.toWidget (stateTreeNodeWidget st)
