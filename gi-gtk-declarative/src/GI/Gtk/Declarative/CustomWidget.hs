{-# LANGUAGE DeriveFunctor #-}

-- | While you can instantiate 'Patchable' and 'EventSource' for your
-- own data types, it's a bit complicated. The 'CustomWidget' data
-- type takes care of some lower-level detail, so that you can focus
-- on the custom behavior of your widget. You still need to think
-- about and implement a patching function, but in an easier way.
module GI.Gtk.Declarative.CustomWidget
  ( CustomPatch(..)
  , CustomWidget(..)
  )
where

import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.State

-- | Similar to 'Patch', describing a possible action to perform on a
-- 'Gtk.Widget', decided by 'customPatch'.
data CustomPatch widget customData
  = CustomReplace
  | CustomModify (widget -> IO SomeState)
  | CustomKeep

-- | A custom widget specification, with all functions needed to
-- instantiate 'Patchable' and 'EventSource'. A custom widget is based
-- on a top 'widget', can use 'customData' as a way of passing
-- parameters, and emits events of type 'event'.
data CustomWidget widget customData event =
  CustomWidget
  { customWidget :: Gtk.ManagedPtr widget -> widget
  -- ^ The widget constructor
  , customCreate :: customData -> IO SomeState
  -- ^ Action that creates the initial widget
  , customPatch :: SomeState -> customData -> customData -> CustomPatch widget customData
  -- ^ Patch function, calculating a 'CustomPatch' based on the state,
  -- old custom data, and new custom data.
  , customSubscribe :: customData -> widget -> (event -> IO ()) -> IO Subscription
  -- ^ Action that creates an event subscription for the custom widget
  , customData :: customData
  -- ^ The custom data (e.g. parameters) of the custom widget
  } deriving (Functor)

instance Gtk.IsWidget widget => Patchable (CustomWidget widget customData) where
  create custom = customCreate custom (customData custom)
  patch state' old new =
    case customPatch old state' (customData old) (customData new) of
      CustomReplace -> Replace (customCreate new (customData new))
      CustomModify f -> Modify (f =<< Gtk.unsafeCastTo (customWidget new) =<< someStateWidget state')
      CustomKeep -> Keep

instance Gtk.GObject widget => EventSource (CustomWidget widget customData) where
  subscribe custom state' cb = do
    w' <- Gtk.unsafeCastTo (customWidget custom) =<< someStateWidget state'
    customSubscribe custom (customData custom) w' cb
