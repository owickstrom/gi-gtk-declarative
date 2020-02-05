-- | Event handling for declarative widgets.
module GI.Gtk.Declarative.EventSource
  ( EventSource(..)
  , module GI.Gtk.Declarative.EventSource.Subscription
  )
where

import           GI.Gtk.Declarative.State
import           GI.Gtk.Declarative.EventSource.Subscription

-- | An 'EventSource' can be subscribed to, with a callback, returning a
-- 'Subscription'.
class EventSource widget where
  subscribe
    :: widget event     -- ^ Declarative widget with event handlers.
    -> SomeState        -- ^ State of rendered widget tree.
    -> (event -> IO ()) -- ^ Event callback, invoked on each emitted event until
                        -- the 'Subscription' is cancelled, or widget is otherwise
                        -- destroyed.
    -> IO Subscription  -- ^ A 'Subscription' is returned, which can be cancelled.
