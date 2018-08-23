module GI.Gtk.Declarative.EventSource
  ( ConnectedHandler(..)
  , Subscription(..)
  , cancel
  , EventSource
  , subscribe
  , joinSubscriptions
  )
where

import           Control.Monad
import           Control.Concurrent
import           Data.Traversable
import qualified GI.Gtk                        as Gtk
import qualified Data.GI.Base.Signals          as GI

data ConnectedHandler = ConnectedHandler Gtk.Widget GI.SignalHandlerId

data Subscription event = Subscription { events :: Chan event, handlers :: [ConnectedHandler] }

cancel :: Subscription event -> IO ()
cancel sub =
    -- TODO: Disconnect signals. Doesn't seem like haskell-gi-base supports this. PR time!
  forM_ (handlers sub) $ \(ConnectedHandler _widget _handlerId) -> return ()

class EventSource widget where
  subscribe :: widget event -> Gtk.Widget -> IO (Subscription event)

joinSubscriptions :: [Subscription event] -> IO (Subscription event)
joinSubscriptions subs = do
  allEvents   <- newChan
  allHandlers <- for subs $ \sub -> do
      -- TODO: This thread ID should be stored and killed later on.
    void . forkIO . forever $ writeChan allEvents =<< readChan (events sub)
    return (handlers sub)
  return (Subscription allEvents (join allHandlers))
