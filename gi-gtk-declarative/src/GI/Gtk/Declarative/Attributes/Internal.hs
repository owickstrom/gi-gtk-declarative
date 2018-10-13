{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

-- | Internal helpers for applying attributes and signal handlers to GTK+
-- widgets.
module GI.Gtk.Declarative.Attributes.Internal
  ( applyAfterCreated
  , extractAttrConstructOps
  , extractAttrSetOps
  , addClass
  , removeClass
  , addSignalHandler
  )
where

import           Control.Monad                            ( (>=>) )
import           Control.Monad.IO.Class                   ( MonadIO )
import qualified Data.GI.Base.Attributes       as GI
import qualified GI.GObject                    as GI
import qualified GI.Gtk                        as Gtk

import           GI.Gtk.Declarative.Attributes
import           GI.Gtk.Declarative.Attributes.Internal.Conversions
import           GI.Gtk.Declarative.EventSource

applyAfterCreated :: widget -> Attribute widget event -> IO ()
applyAfterCreated widget = \case
  (AfterCreated f) -> f widget
  _                -> return ()

extractAttrConstructOps
  :: Attribute widget event -> [GI.AttrOp widget 'GI.AttrConstruct]
extractAttrConstructOps = \case
  (attr := value) -> pure (attr Gtk.:= value)
  _               -> mempty

extractAttrSetOps :: Attribute widget event -> [GI.AttrOp widget 'GI.AttrSet]
extractAttrSetOps = \case
  (attr := value) -> pure (attr Gtk.:= value)
  _               -> mempty

addClass :: MonadIO m => Gtk.StyleContext -> Attribute widget event -> m ()
addClass sc = \case
  Classes cs -> mapM_ (Gtk.styleContextAddClass sc) cs
  _          -> pure ()

removeClass :: MonadIO m => Gtk.StyleContext -> Attribute widget event -> m ()
removeClass sc = \case
  Classes cs -> mapM_ (Gtk.styleContextRemoveClass sc) cs
  _          -> pure ()

addSignalHandler
  :: (Gtk.IsWidget widget, MonadIO m)
  => (event -> IO ())
  -> widget
  -> Attribute widget event
  -> m (Maybe Subscription)
addSignalHandler onEvent widget' = listenToSignal >=> \case
  Just cb -> setupCancellation cb
  Nothing -> pure Nothing
 where
  listenToSignal = \case
    OnSignalPure signal callback ->
      Just <$> Gtk.on widget' signal (toGtkCallback callback widget' onEvent)
    OnSignalImpure signal callback ->
      Just <$> Gtk.on widget' signal (toGtkCallback callback widget' onEvent)
    _ -> pure Nothing
  setupCancellation handlerId = do
    w <- Gtk.toWidget widget'
    pure (Just (fromCancellation (GI.signalHandlerDisconnect w handlerId)))
