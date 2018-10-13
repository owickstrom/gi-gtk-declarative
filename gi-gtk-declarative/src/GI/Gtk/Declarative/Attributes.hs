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
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}

-- | Attribute lists on declarative objects, supporting the underlying
-- attributes from "Data.GI.Base.Attributes", along with CSS class lists, and
-- pure and impure event callbacks.

module GI.Gtk.Declarative.Attributes
  ( Attribute(..)
  , classes
  , afterCreated
  -- * Event Handling
  , on
  , onM
  -- * Callbacks
  , Callback(..)
  , ToGtkCallback(..)
  )
where

import           Control.Monad           (void)
import           Data.Functor.Identity
import qualified Data.GI.Base.Attributes as GI
import qualified Data.GI.Base.Signals    as GI
import qualified Data.HashSet            as HashSet
import qualified Data.Text               as T
import           Data.Typeable
import           GHC.TypeLits            (KnownSymbol, Symbol)
import qualified GI.Gtk                  as Gtk

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
  -- | Emit events using a pure callback. Use the 'on' function, instead of this
  -- constructor directly.
  OnSignalPure
    :: ( Gtk.GObject widget
       , GI.SignalInfo info
       , gtkCallback ~ GI.HaskellCallbackType info
       , ToGtkCallback gtkCallback widget Pure
       )
    => Gtk.SignalProxy widget info
    -> Callback gtkCallback widget Pure event
    -> Attribute widget event
  -- | Emit events using a pure callback. Use the 'on' function, instead of this
  -- constructor directly.
  OnSignalImpure
    :: ( Gtk.GObject widget
       , GI.SignalInfo info
       , gtkCallback ~ GI.HaskellCallbackType info
       , ToGtkCallback gtkCallback widget Impure
       )
    => Gtk.SignalProxy widget info
    -> Callback gtkCallback widget Impure event
    -> Attribute widget event
  -- | Provide a callback to modify the widget after it's been created.
  AfterCreated
    :: (widget -> IO ())
    -> Attribute widget event

-- | Attributes have a 'Functor' instance that maps events in all event
-- callbacks.
instance Functor (Attribute widget) where
  fmap f = \case
    attr := value -> attr := value
    Classes cs -> Classes cs
    OnSignalPure signal cb -> OnSignalPure signal (fmap f cb)
    OnSignalImpure signal cb -> OnSignalImpure signal (fmap f cb)
    AfterCreated cb -> AfterCreated cb

-- | Define the CSS classes for the underlying widget's style context. For these
-- classes to have any effect, this requires a 'Gtk.CssProvider' with CSS files
-- loaded, to be added to the GDK screen. You probably want to do this in your
-- entry point when setting up GTK.
classes :: Gtk.IsWidget widget => [T.Text] -> Attribute widget event
classes = Classes . HashSet.fromList

-- | Emit events, using a pure callback, by subcribing to the specified
-- signal.
on
  :: ( Gtk.GObject widget
     , GI.SignalInfo info
     , gtkCallback ~ GI.HaskellCallbackType info
     , ToGtkCallback gtkCallback widget Pure
     , ToCallback gtkCallback widget Pure event
     , userCallback ~ UserCallback gtkCallback widget Pure event
     )
  => Gtk.SignalProxy widget info
  -> userCallback
  -> Attribute widget event
on signal = OnSignalPure signal . toCallback

-- | Emit events, using an impure callback receiving the 'widget' and returning
-- an 'IO' action of 'event', by subcribing to the specified signal.
onM
  :: ( Gtk.GObject widget
     , GI.SignalInfo info
     , gtkCallback ~ GI.HaskellCallbackType info
     , ToGtkCallback gtkCallback widget Impure
     , ToCallback gtkCallback widget Impure event
     , userCallback ~ UserCallback gtkCallback widget Impure event
     )
  => Gtk.SignalProxy widget info
  -> userCallback -- (Callback gtkCallback widget Impure event)
  -> Attribute widget event
onM signal = OnSignalImpure signal . toCallback

-- | Provide a callback to modify the widget after it's been created.
afterCreated :: (widget -> IO ()) -> Attribute widget event
afterCreated = AfterCreated

-- * Callbacks

-- | A 'Callback' can be either 'Pure' or 'Impure'.
data Purity = Pure | Impure

data CallbackReturn m gtkReturn event where
  OnlyEvent :: m e -> CallbackReturn m () e
  ReturnAndEvent :: m (Bool, e) -> CallbackReturn m Bool e

instance Functor m => Functor (CallbackReturn m gtkCallback) where
  fmap f = \case
    OnlyEvent e -> OnlyEvent (fmap f e)
    ReturnAndEvent mr -> ReturnAndEvent (fmap (fmap f) mr)

-- | A callback type that encodes the user callback in a way that we can have
-- a 'Functor' instance for arbitrary-arity callbacks.
data Callback gtkCallback widget (purity :: Purity) event where
  PureCallback :: CallbackReturn Identity ret e -> Callback (IO ret) w Pure e
  ImpureCallback :: (w -> CallbackReturn IO ret e) -> Callback (IO ret) w Impure e
  CallbackFunction :: (a -> Callback b w p e) -> Callback (a -> b) w p e

instance Functor (Callback gtkCallback widget purity) where
  fmap f = \case
    PureCallback r -> PureCallback (fmap f r)
    ImpureCallback r -> ImpureCallback (fmap (fmap f) r)
    CallbackFunction cb -> CallbackFunction (\a -> fmap f (cb a))

-- | Convert from a GTK+ callback type to a user callback type (the ones
-- you'd apply 'on' and 'onM' with) based on the given widget, purity, and event
-- types.
type family UserCallback gtkCallback widget (purity :: Purity) event where
  UserCallback (IO ())   widget Pure   event = event
  UserCallback (IO Bool) widget Pure   event = (Bool, event)
  UserCallback (IO ())   widget Impure event = widget -> IO event
  UserCallback (IO Bool) widget Impure event = widget -> IO (Bool, event)
  UserCallback (a -> b)  widget purity event = a -> UserCallback b widget purity event

-- | Internal class for converting user callbacks to 'Callback' values.
class ToCallback gtkCallback widget purity event where
  -- | Convert from a user callback type to a 'Callback'.
  toCallback
    :: UserCallback gtkCallback widget purity event
    -> Callback gtkCallback widget purity event

instance ToCallback (IO ()) widget Pure event where
  toCallback = PureCallback . OnlyEvent . pure

instance ToCallback (IO Bool) widget Pure event where
  toCallback = PureCallback . ReturnAndEvent . pure

instance ToCallback (IO ()) widget Impure event where
  toCallback cb = ImpureCallback (OnlyEvent . cb)

instance ToCallback (IO Bool) widget Impure event where
  toCallback cb = ImpureCallback (ReturnAndEvent . cb)

instance (ToCallback b widget purity event)
  => ToCallback (a -> b) widget purity event where
  toCallback f = CallbackFunction (toCallback . f)


-- * GTK+ Callback Conversions

-- | Internal class for converting 'Callback's to gi-gtk callbacks.
class ToGtkCallback gtkCallback widget purity where
  -- | Converts a 'Callback', i.e. the internal encoding of a pure or an impure
  -- callback, back to a GTK+ callback. Impure callbacks will also receive a
  -- 'widget' as the last argument.
  toGtkCallback
    :: Callback gtkCallback widget purity event
    -> widget
    -> (event -> IO ())
    -> gtkCallback

instance ToGtkCallback (IO ()) widget Pure where
  toGtkCallback (PureCallback (OnlyEvent e)) _ f = void (f (runIdentity e))

instance ToGtkCallback (IO Bool) widget Pure where
  toGtkCallback (PureCallback (ReturnAndEvent re)) _ f =
    let (r, e) = runIdentity re
    in f e *> return r

instance ToGtkCallback (IO ()) widget Impure where
  toGtkCallback (ImpureCallback r) w f =
    let OnlyEvent me = r w
    in me >>= f

instance ToGtkCallback (IO Bool) widget Impure where
  toGtkCallback (ImpureCallback r) w f = do
    let ReturnAndEvent re = r w
    (r', e) <- re
    f e
    return r'

instance ToGtkCallback y widget purity => ToGtkCallback (x -> y) widget purity where
  toGtkCallback (CallbackFunction cb) f w x = toGtkCallback (cb x) f w
