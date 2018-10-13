{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module GI.Gtk.Declarative.Attributes.Internal.Callback
  ( Purity(..)
  , CallbackReturn(..)
  , Callback(..)
  , UserCallback
  , ToCallback(..)
  )
where

import           Data.Functor.Identity

-- | A 'Callback' can be either pure or impure.
data Purity = Pure | Impure

-- | The two supported types of return values in user callbacks are encoded
-- by the 'CallbackReturn' type; either you can return only an 'event', or if
-- the underlying GTK+ signal handler needs to return a 'Bool', you return
-- a @(Bool, event)@ tuple.
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
class ToCallback gtkCallback widget purity where
  -- | Convert from a user callback type to a 'Callback'.
  toCallback
    :: UserCallback gtkCallback widget purity event
    -> Callback gtkCallback widget purity event

instance ToCallback (IO ()) widget Pure where
  toCallback = PureCallback . OnlyEvent . pure

instance ToCallback (IO Bool) widget Pure where
  toCallback = PureCallback . ReturnAndEvent . pure

instance ToCallback (IO ()) widget Impure where
  toCallback cb = ImpureCallback (OnlyEvent . cb)

instance ToCallback (IO Bool) widget Impure where
  toCallback cb = ImpureCallback (ReturnAndEvent . cb)

instance (ToCallback b widget purity) => ToCallback (a -> b) widget purity where
  toCallback f = CallbackFunction (toCallback . f)
