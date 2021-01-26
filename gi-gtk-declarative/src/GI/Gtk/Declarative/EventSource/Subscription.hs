{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GI.Gtk.Declarative.EventSource.Subscription
  ( Subscription
  , fromCancellation
  , cancel
  )
where

import           Data.Vector (Vector)

-- | Cancel a 'Subscription', meaning that the callback will not be invoked on
-- any subsequent signal emissions.
cancel :: Subscription -> IO ()
cancel = sequence_ . cancellations

-- | A 'Subscription' contains zero or more cancellation actions for connected
-- handlers (to a tree of widgets.) When subscribing to a container widget, all
-- child widgets are also subscribed to, and the 'Subscription's are combined
-- using the 'Semigroup' instance.
newtype Subscription = Subscription { cancellations :: Vector (IO ()) }
  deriving (Semigroup, Monoid)

-- | Create a subscription from a cancellation IO action.
fromCancellation :: IO () -> Subscription
fromCancellation = Subscription . pure
