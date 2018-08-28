{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

-- | Property lists on declarative objects, supporting the underlying
-- attributes from "Data.GI.Base.Attributes", along with pure and impure event
-- callbacks.

module GI.Gtk.Declarative.Props
  ( ToGtkCallback
  , toGtkCallback
  , PropPair(..)
  , classes
  , on
  , onM
  -- Helpers
  , extractAttrConstructOps
  , extractAttrSetOps
  , addClass
  , removeClass
  , addSignalHandler
  )
where

import           Control.Monad                  (void)
import           Control.Monad.IO.Class         (MonadIO)
import qualified Data.GI.Base.Attributes        as GI
import qualified Data.GI.Base.Signals           as GI
import qualified Data.HashSet                   as HashSet
import           Data.Text                      (Text)
import           Data.Typeable
import           GHC.TypeLits                   (KnownSymbol, Symbol)
import qualified GI.Gtk                         as Gtk

import           GI.Gtk.Declarative.CSS
import           GI.Gtk.Declarative.EventSource

infix 5 :=

-- * Pure Callbacks

-- | Convert a GTK+ callback type to a pure callback type, i.e. a type
-- without 'IO'. The pure callback is either a single 'event', or a function of
-- the same arity and arguments as the GTK+ callback, but with the 'event' as
-- the range.
type family ToPureCallback gtkCallback event where
  ToPureCallback (IO ()) event = event
  ToPureCallback (a -> b) event = a -> ToPureCallback b event

-- | A 'PureCallback' holds a pure callback, as defined by 'ToPureCallback'.
data PureCallback callback event where
  PureCallback
    :: (pure ~ ToPureCallback callback event)
    => pure
    -> PureCallback callback event

-- The functor instances are pretty annoying, being repeated for each function
-- arity. Could this be done in another way?

instance Functor (PureCallback (IO ())) where
  fmap f (PureCallback e) = PureCallback (f e)

instance Functor (PureCallback (x -> IO ())) where
  fmap f (PureCallback g) = PureCallback (f . g)

instance Functor (PureCallback (x -> y -> IO ())) where
  fmap f (PureCallback g) = PureCallback (\x -> f . g x)

-- * Impure Callbacks

-- | Convert a GTK+ callback type to an impure callback type, i.e. a type
-- with 'IO event' as the range, instead of 'IO ()'. The impure callback is
-- either a single 'IO event', or a function of the same arity and arguments as
-- the GTK+ callback, but with 'IO event' as the range.
type family ToImpureCallback t e where
  ToImpureCallback (IO ()) e  = IO e
  ToImpureCallback (a -> b) e = a -> ToImpureCallback b e

-- | An 'ImpureCallback' holds an impure callback, as defined by
-- 'ToImpureCallback', but with an extra 'widget' argument. This is so that
-- impure callbacks can query their underlying GTK+ widgets for data.
data ImpureCallback callback widget event where
  ImpureCallback
    :: (impure ~ ToImpureCallback callback event)
    => (widget -> impure)
    -> ImpureCallback callback widget event

-- The functor instances are pretty annoying, being repeated for each function
-- arity. Could this be done in another way?

instance Functor (ImpureCallback (IO ()) widget) where
  fmap f (ImpureCallback g) = ImpureCallback (\w -> f <$> g w)

instance Functor (ImpureCallback (x -> IO ()) widget) where
  fmap f (ImpureCallback g) = ImpureCallback (\w -> fmap f . g w)

instance Functor (ImpureCallback (x -> y -> IO ()) widget) where
  fmap f (ImpureCallback g) = ImpureCallback (\w x -> fmap f . g w x)

-- * GTK+ Callback Conversions

-- | Converts a user callback, i.e. a pure or an impure callback, back to a
-- GTK+ callback.
class ToGtkCallback userCallback where
  type CustomGtkCallback userCallback :: *
  toGtkCallback :: userCallback event -> (event -> IO ()) -> CustomGtkCallback userCallback

instance ToGtkCallback (PureCallback (IO ())) where
  type CustomGtkCallback (PureCallback (IO ())) = IO ()
  toGtkCallback (PureCallback cb) f = void (f cb)

instance ToGtkCallback (PureCallback (x -> IO ())) where
  type CustomGtkCallback (PureCallback (x -> IO ())) = x -> IO ()
  toGtkCallback (PureCallback cb) f x = void (f (cb x))

instance ToGtkCallback (PureCallback (x -> y -> IO ()))  where
  type CustomGtkCallback (PureCallback (x -> y -> IO ())) = x -> y -> IO ()
  toGtkCallback (PureCallback cb) f x y = void (f (cb x y))

instance ToGtkCallback (ImpureCallback (IO ()) widget)  where
  type CustomGtkCallback (ImpureCallback (IO ()) widget) = widget -> IO ()
  toGtkCallback (ImpureCallback cb) f w = void (cb w >>= f)

-- * Props

data PropPair widget event where
  (:=)
    :: (GI.AttrOpAllowed 'GI.AttrConstruct info widget
      , GI.AttrOpAllowed 'GI.AttrSet info widget
      , GI.AttrGetC info widget attr getValue
      , GI.AttrSetTypeConstraint info setValue
      , KnownSymbol attr
      , Typeable attr
      )
   =>  GI.AttrLabelProxy (attr :: Symbol) -> setValue -> PropPair widget event
  Classes
    :: Gtk.IsWidget widget
    => ClassSet
    -> PropPair widget event
  OnSignalPure
    :: ( Gtk.GObject widget
       , GI.SignalInfo info
       , callback ~ GI.HaskellCallbackType info
       , Functor (PureCallback callback)
       , ToGtkCallback (PureCallback callback)
       , callback ~ CustomGtkCallback (PureCallback callback)
       )
    => Gtk.SignalProxy widget info
    -> PureCallback callback event
    -> PropPair widget event
  OnSignalImpure
    :: ( Gtk.GObject widget
       , GI.SignalInfo info
       , callback ~ GI.HaskellCallbackType info
       , Functor (ImpureCallback callback widget)
       , ToGtkCallback (ImpureCallback callback widget)
       , (widget -> callback) ~ CustomGtkCallback (ImpureCallback callback widget)
       )
    => Gtk.SignalProxy widget info
    -> ImpureCallback callback widget event
    -> PropPair widget event

instance Functor (PropPair widget) where
  fmap f prop =
    case prop of
      (attr := value) -> (attr := value)
      (Classes cs) -> Classes cs
      OnSignalPure signal callback -> OnSignalPure signal (fmap f callback)
      OnSignalImpure signal callback -> OnSignalImpure signal (fmap f callback)

classes :: Gtk.IsWidget widget => [Text] -> PropPair widget event
classes = Classes . HashSet.fromList

-- | Publish events, using a pure callback, by subcribing to the specified
-- signal.
on
  :: ( Gtk.GObject widget
     , GI.SignalInfo info
     , callback ~ GI.HaskellCallbackType info
     , pure ~ ToPureCallback callback event
     , Functor (PureCallback callback)
     , ToGtkCallback (PureCallback callback)
     , callback ~ CustomGtkCallback (PureCallback callback)
     )
  => Gtk.SignalProxy widget info
  -> pure
  -> PropPair widget event
on signal = OnSignalPure signal . PureCallback

-- | Publish events, using an impure callback, by subcribing to the specified
-- signal.
onM
  :: ( Gtk.GObject widget
     , GI.SignalInfo info
     , callback ~ GI.HaskellCallbackType info
     , impure ~ ToImpureCallback callback event
     , withWidget ~ (widget -> impure)
     , Functor (ImpureCallback callback widget)
     , ToGtkCallback (ImpureCallback callback widget)
     , (widget -> callback) ~ CustomGtkCallback (ImpureCallback callback widget)
     )
  => Gtk.SignalProxy widget info
  -> withWidget
  -> PropPair widget event
onM signal = OnSignalImpure signal . ImpureCallback

-- * Props helpers

extractAttrConstructOps
  :: PropPair widget event -> [GI.AttrOp widget 'GI.AttrConstruct]
extractAttrConstructOps = \case
  (attr := value) -> pure (attr Gtk.:= value)
  _               -> mempty

extractAttrSetOps :: PropPair widget event -> [GI.AttrOp widget 'GI.AttrSet]
extractAttrSetOps = \case
  (attr := value) -> pure (attr Gtk.:= value)
  _               -> mempty

addClass :: MonadIO m => Gtk.StyleContext -> PropPair widget event -> m ()
addClass sc = \case
  Classes cs -> mapM_ (Gtk.styleContextAddClass sc) cs
  _          -> pure ()

removeClass :: MonadIO m => Gtk.StyleContext -> PropPair widget event -> m ()
removeClass sc = \case
  Classes cs -> mapM_ (Gtk.styleContextRemoveClass sc) cs
  _          -> pure ()

addSignalHandler
  :: (Gtk.IsWidget widget, MonadIO m)
  => (event -> IO ())
  -> widget
  -> PropPair widget event
  -> m (Maybe ConnectedHandler)
addSignalHandler onEvent widget' = \case
  OnSignalPure signal handler -> do
    handlerId <- Gtk.on widget' signal (toGtkCallback handler onEvent)
    w         <- Gtk.toWidget widget'
    pure (Just (ConnectedHandler w handlerId))
  OnSignalImpure signal handler -> do
    handlerId <- Gtk.on widget' signal (toGtkCallback handler onEvent widget')
    w         <- Gtk.toWidget widget'
    pure (Just (ConnectedHandler w handlerId))
  _ -> pure Nothing

