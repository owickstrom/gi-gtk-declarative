{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GI.Gtk.Declarative.EventSource
  ( ConnectedHandler(..)
  , Subscription(..)
  , cancel
  , EventSource
  , subscribe
  )
where

import           Control.Monad
import qualified GI.Gtk                        as Gtk
import qualified Data.GI.Base.Signals          as GI

-- | A handler connected to a 'Subscription'.
data ConnectedHandler = ConnectedHandler Gtk.Widget GI.SignalHandlerId

-- | A 'Subscription' contains zero or more connected handlers for a tree of
-- widgets. When subscribing to a container widget, all child widgets are
-- also subscribed to, and the 'Subscription's are combined using the
-- 'Semigroup' instance.
newtype Subscription = Subscription { handlers :: [ConnectedHandler] }
  deriving (Semigroup, Monoid)

-- | Cancel a 'Subscription', meaning that the callback will not be invoked on
-- any subsequent signal emissions.
cancel :: Subscription -> IO ()
cancel sub =
    -- TODO: Disconnect signals. Doesn't seem like haskell-gi-base supports this. PR time!
  forM_ (handlers sub) $ \(ConnectedHandler _widget _handlerId) -> return ()

-- | An 'EventSource' can be subscribed to, with a callback, returning a
-- 'Subscription'.
class EventSource widget event | widget -> event where
  subscribe :: widget -> Gtk.Widget -> (event -> IO ()) -> IO Subscription
