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

-- | Attribute lists on declarative objects, supporting the underlying
-- attributes from "Data.GI.Base.Attributes", along with CSS class lists, and
-- pure and impure event callbacks.

module GI.Gtk.Declarative.Attributes
  ( Attribute(..)
  -- *
  , classes
  -- * Event Handling
  , on
  , onM
  -- * Callbacks
  , ToGtkCallback(..)
  )
where

import           Control.Monad                  (void)
import qualified Data.GI.Base.Attributes        as GI
import qualified Data.GI.Base.Signals           as GI
import qualified Data.HashSet                   as HashSet
import           Data.Text                      (Text)
import           Data.Typeable
import           GHC.TypeLits                   (KnownSymbol, Symbol)
import qualified GI.Gtk                         as Gtk

import           GI.Gtk.Declarative.CSS

-- * Attributes

-- | The attribute GADT represent a supported attribute for a declarative
-- widget. This extends the regular notion of GTK+ attributes to also include
-- event handling and CSS classes.
data Attribute widget event where
  -- | An attribute/value mapping for a declarative widget. The
  -- 'GI.AttrLabelProxy' is parameterized by 'attr', which represents the
  -- GTK-defined attribute name. The underlying GI object needs to support
  -- the /construct/, /get/, and /set/ operations for the given attribute.
  (:=)
    :: (GI.AttrOpAllowed 'GI.AttrConstruct info widget
      , GI.AttrOpAllowed 'GI.AttrSet info widget
      , GI.AttrGetC info widget attr getValue
      , GI.AttrSetTypeConstraint info setValue
      , KnownSymbol attr
      , Typeable attr
      )
   => GI.AttrLabelProxy (attr :: Symbol) -> setValue -> Attribute widget event
  -- | Defines a set of CSS classes for the underlying widget's style context.
  -- Use the 'classes' function instead of this constructor directly.
  Classes
    :: Gtk.IsWidget widget
    => ClassSet
    -> Attribute widget event
  -- | Emit events using a pure callback. Use the 'on function instead of this
  -- constructor directly.
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
    -> Attribute widget event
  -- | Emit events using an impure callback. Use the 'on function instead of
  -- this constructor directly.
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
    -> Attribute widget event

-- | Attributes have a 'Functor' instance that maps events in all event
-- callbacks.
instance Functor (Attribute widget) where
  fmap f = \case
    attr := value -> attr := value
    Classes cs -> Classes cs
    OnSignalPure signal cb -> OnSignalPure signal (fmap f cb)
    OnSignalImpure signal cb -> OnSignalImpure signal (fmap f cb)

-- | Define the CSS classes for the underlying widget's style context. For these
-- classes to have any effect, this requires a 'Gtk.CssProvider' with CSS files
-- loaded, to be added to the GDK screen. You probably want to do this in your
-- entry point when setting up GTK.
classes :: Gtk.IsWidget widget => [Text] -> Attribute widget event
classes = Classes . HashSet.fromList

-- | Emit events, using a pure callback, by subcribing to the specified
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
  -> Attribute widget event
on signal = OnSignalPure signal . PureCallback

-- | Emit events, using an impure callback receiving the 'widget' and returning
-- an 'IO' action of 'event', by subcribing to the specified signal.
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
  -> Attribute widget event
onM signal = OnSignalImpure signal . ImpureCallback

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

-- | Internal class for converting user callbacks to gi-gtk callbacks.
class ToGtkCallback userCallback where
  type CustomGtkCallback userCallback :: *
  -- | Converts a user callback, i.e. a pure or an impure callback, back to a
  -- GTK+ callback.
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
