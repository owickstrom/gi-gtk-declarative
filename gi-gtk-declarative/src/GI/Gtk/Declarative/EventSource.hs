{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- | Event handling for declarative widgets.
module GI.Gtk.Declarative.EventSource
  ( EventSource(..)
  , Subscription
  , fromCancellation
  , cancel
  )
where

import           Data.Vector                    ( Vector )

import           GI.Gtk.Declarative.State

-- | Cancel a 'Subscription', meaning that the callback will not be invoked on
-- any subsequent signal emissions.
cancel :: Subscription -> IO ()
cancel = sequence_ . cancellations

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

-- | A 'Subscription' contains zero or more cancellation actions for connected
-- handlers (to a tree of widgets.) When subscribing to a container widget, all
-- child widgets are also subscribed to, and the 'Subscription's are combined
-- using the 'Semigroup' instance.
newtype Subscription = Subscription { cancellations :: Vector (IO ()) }
  deriving (Semigroup, Monoid)

-- | Create a subscription from a cancellation IO action.
fromCancellation :: IO () -> Subscription
fromCancellation = Subscription . pure
