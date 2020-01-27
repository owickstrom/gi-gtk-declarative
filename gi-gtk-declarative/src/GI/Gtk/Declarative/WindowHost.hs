{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

module GI.Gtk.Declarative.WindowHost (windowHost) where

-- | A wrapper around a child widget that also allows you to create a new window.
-- The window itself is not a child of this widget, nor of the parent widget, but is
-- a new top-level window: The window host just provides a place for the new window
-- to live, whilst fitting into the general tree-of-components pattern that is used
-- by gi-gtk-declarative.
import           Data.Maybe                     (fromMaybe)
import           Data.Typeable                  ((:~:) (..), Typeable, eqT)
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative.Bin         (Bin (..))
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.State
import           GI.Gtk.Declarative.Widget

-- | Construct a /windowHost/ widget.
windowHost
  :: Maybe (Bin Gtk.Window event) -- ^ An optional window, which will be a new top-level widget.
  -> Widget event                 -- ^ A child widget to include in the normal widget tree.
  -> Widget event                 -- ^ The child widget, optionally with a window linked to it.
windowHost window child = Widget $ WindowHost window child

data WindowHost event =
  WindowHost (Maybe (Bin Gtk.Window event)) (Widget event)
  deriving (Functor)

data WindowState = forall a. Typeable a => WindowState (Maybe SomeState) a
   deriving (Typeable)

instance Patchable WindowHost where

  create (WindowHost window child) = do
    wrapState <$> traverse create window <*> create child

  patch state (WindowHost w1 c1) (WindowHost w2 c2)
    | Just (windowState, childState) <- unwrapState state =
        patch' windowState childState (w1, c1) (w2, c2)
    | otherwise =
        Replace . create $ WindowHost w2 c2

  destroy state (WindowHost w c)
    | Just (windowState, childState) <- unwrapState state = do
        destroy childState c
        case (windowState, w) of
          (Just ws, Just w') -> destroy ws w'
          (Nothing, Nothing) -> pure ()
          _                  -> error "Declarative window widget and state do not match"
    | otherwise =
        error "Cannot destroy WindowHost with a non-WindowState state tree"

patch'
  :: Maybe SomeState
  -> SomeState
  -> (Maybe (Bin Gtk.Window e1), Widget e1)
  -> (Maybe (Bin Gtk.Window e2), Widget e2)
  -> Patch
patch' windowState childState (w1, c1) (w2, c2) = case (w1, w2, windowState) of
  (Just _  , _       , Nothing          ) -> error "Previous window but no previous state"
  (Nothing , _       , Just _           ) -> error "Previous state but no previous window"
  (Just w1', Just w2', Just windowState') -> patch'' windowState' w1' w2'
  (Just w  , Nothing , Just ws          ) -> destroyWindow ws w
  (Nothing , Just w2', Nothing          ) -> modifyWindow $ create w2'
  (Nothing , Nothing , Nothing          ) -> keepWindow Nothing
 where
  patch'' :: SomeState -> Bin Gtk.Window e1 -> Bin Gtk.Window e2 -> Patch
  patch'' windowState' w1' w2' = case patch windowState' w1' w2' of
    Keep       -> keepWindow $ Just windowState'
    Modify  wp -> modifyWindow wp
    Replace wp -> modifyWindow $ destroy windowState' w1' *> wp

  keepWindow :: Maybe SomeState -> Patch
  keepWindow windowState' = case patch childState c1 c2 of
    Keep       -> Keep
    Modify  cs -> Modify $ wrapState windowState' <$> cs
    Replace cs -> Replace $ wrapState windowState' <$> cs

  modifyWindow :: IO SomeState -> Patch
  modifyWindow windowPatch = case patch childState c1 c2 of
    Keep       -> Modify $ flip wrapState childState . Just <$> windowPatch
    Modify  cs -> Modify $ wrapState <$> fmap Just windowPatch <*> cs
    Replace cs -> Replace $ wrapState <$> fmap Just windowPatch <*> cs

  destroyWindow :: SomeState -> Bin Gtk.Window e -> Patch
  destroyWindow windowState' window = case patch childState c1 c2 of
    Keep       -> Modify $ wrapState Nothing childState <$ destroy windowState' window
    Modify  cs -> Modify $ destroy windowState' window *> (wrapState Nothing <$> cs)
    Replace cs -> Replace $ destroy windowState' window *> (wrapState Nothing <$> cs)

-- | Wrap the child state in the window state
wrapState :: Maybe SomeState -> SomeState -> SomeState
wrapState windowState (SomeState childStateTree) = case childStateTree of
  StateTreeWidget node    -> SomeState $ StateTreeWidget $ wrap node
  StateTreeBin node child -> SomeState $ StateTreeBin (wrap node) child
  StateTreeContainer node children ->
    SomeState $ StateTreeContainer (wrap node) children
 where
  wrap :: Typeable c => StateTreeNode w e c -> StateTreeNode w e WindowState
  wrap node = node
    { stateTreeCustomState = WindowState windowState (stateTreeCustomState node)
    }

-- | Separate the window state and child state
unwrapState :: SomeState -> Maybe (Maybe SomeState, SomeState)
unwrapState (SomeState (st :: StateTree st w c e cs)) =
  case (eqT @cs @WindowState, stateTreeCustomState $ stateTreeNode st) of
    (Just Refl, WindowState ws orig) -> case st of
      StateTreeWidget node -> Just
        (ws, SomeState $ StateTreeWidget node { stateTreeCustomState = orig })
      StateTreeBin node child ->
        Just
          ( ws
          , SomeState $ StateTreeBin node { stateTreeCustomState = orig } child
          )
      StateTreeContainer node children -> Just
        ( ws
        , SomeState
          $ StateTreeContainer node { stateTreeCustomState = orig } children
        )
    (Nothing, _) -> Nothing

instance EventSource WindowHost where
    subscribe (WindowHost window child) state cb
      | Just (windowState, childState) <- unwrapState state = do
          let windowSubscribe = subscribe <$> window <*> windowState <*> pure cb
          ws <- fromMaybe (pure mempty) windowSubscribe
          cs <- subscribe child childState cb
          pure $ ws <> cs
      | otherwise =
          error "Cannot subscribe to WindowHost events with a non-WindowState state tree"
