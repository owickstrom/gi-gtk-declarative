{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

-- | Internal helpers for applying attributes and signal handlers to GTK+
-- widgets.
module GI.Gtk.Declarative.Attributes.Internal
  ( addSignalHandler
  )
where

import           Control.Monad                  ( (>=>) )
import           Control.Monad.IO.Class         ( MonadIO )
import qualified GI.GObject                    as GI
import qualified GI.Gtk                        as Gtk

import           GI.Gtk.Declarative.Attributes
import           GI.Gtk.Declarative.Attributes.Internal.Conversions
import           GI.Gtk.Declarative.EventSource

addSignalHandler
  :: (Gtk.IsWidget widget, MonadIO m)
  => (event -> IO ())
  -> widget
  -> Attribute widget event
  -> m Subscription
addSignalHandler onEvent widget' = listenToSignal >=> \case
  Just eh -> setupCancellation eh
  Nothing -> pure mempty
 where
  listenToSignal = \case
    OnSignalPure signal handler ->
      Just <$> Gtk.on widget' signal (toGtkCallback handler widget' onEvent)
    OnSignalImpure signal handler ->
      Just <$> Gtk.on widget' signal (toGtkCallback handler widget' onEvent)
    _ -> pure Nothing
  setupCancellation handlerId = do
    w <- Gtk.toWidget widget'
    pure (fromCancellation (GI.signalHandlerDisconnect w handlerId))
