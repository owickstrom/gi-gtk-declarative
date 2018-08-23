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

data ConnectedHandler = ConnectedHandler Gtk.Widget GI.SignalHandlerId

newtype Subscription = Subscription { handlers :: [ConnectedHandler] }
  deriving (Semigroup, Monoid)

cancel :: Subscription -> IO ()
cancel sub =
    -- TODO: Disconnect signals. Doesn't seem like haskell-gi-base supports this. PR time!
  forM_ (handlers sub) $ \(ConnectedHandler _widget _handlerId) -> return ()

class EventSource widget where
  subscribe :: widget event -> Gtk.Widget -> (event -> IO ()) -> IO Subscription
