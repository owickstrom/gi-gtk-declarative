module GI.Gtk.Declarative.State where

import qualified GI.Gtk as Gtk

data StateTree
  = StateTreeWidget StateTreeNode
  | StateTreeBin StateTreeNode StateTree
  | StateTreeContainer StateTreeNode [StateTree]

data StateTreeNode = StateTreeNode
  { stateTreeWidget :: !Gtk.Widget
  , stateTreeStyleContext :: !Gtk.StyleContext
  }

stateTreeNode :: StateTree -> StateTreeNode
stateTreeNode (StateTreeWidget s) = s
stateTreeNode (StateTreeBin s _) = s
stateTreeNode (StateTreeContainer s _) = s

stateTreeNodeWidget :: StateTree -> Gtk.Widget
stateTreeNodeWidget = stateTreeWidget . stateTreeNode
