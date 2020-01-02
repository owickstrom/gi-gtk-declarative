{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module GI.Gtk.Declarative.Attributes.Internal.EventHandler
  ( Purity(..)
  , EventHandlerReturn(..)
  , EventHandler(..)
  , UserEventHandler
  , ToEventHandler(..)
  )
where

import           Data.Functor.Identity

-- | A 'EventHandler' can be either pure or impure.
data Purity = Pure | Impure

-- | The two supported types of return values in user event handlers are encoded
-- by the 'EventHandlerReturn' type; either you can return only an 'event', or if
-- the underlying GTK+ callback needs to return a 'Bool', you return
-- a @(Bool, event)@ tuple.
data EventHandlerReturn m gtkReturn event where
  OnlyEvent ::m e -> EventHandlerReturn m () e
  ReturnAndEvent ::m (Bool, e) -> EventHandlerReturn m Bool e

instance Functor m => Functor (EventHandlerReturn m gtkEventHandler) where
  fmap f = \case
    OnlyEvent      e  -> OnlyEvent (fmap f e)
    ReturnAndEvent mr -> ReturnAndEvent (fmap (fmap f) mr)

-- | Encodes the user event handler in such a way that we can have
-- a 'Functor' instance for arity-polymorphic event handlers.
data EventHandler gtkEventHandler widget (purity :: Purity) event where
  PureEventHandler ::EventHandlerReturn Identity ret e -> EventHandler (IO ret) w Pure e
  ImpureEventHandler ::(w -> EventHandlerReturn IO ret e) -> EventHandler (IO ret) w Impure e
  EventHandlerFunction ::(a -> EventHandler b w p e) -> EventHandler (a -> b) w p e

instance Functor (EventHandler gtkEventHandler widget purity) where
  fmap f = \case
    PureEventHandler     r  -> PureEventHandler (fmap f r)
    ImpureEventHandler   r  -> ImpureEventHandler (fmap (fmap f) r)
    EventHandlerFunction eh -> EventHandlerFunction (\a -> fmap f (eh a))

-- | Convert from a GTK+ callback type to a user event handler type (the ones
-- you'd apply 'on' and 'onM' with) based on the given widget, purity, and event
-- types.
type family UserEventHandler gtkCallback widget (purity :: Purity) event where
  UserEventHandler (IO ())   widget Pure   event = event
  UserEventHandler (IO Bool) widget Pure   event = (Bool, event)
  UserEventHandler (IO ())   widget Impure event = widget -> IO event
  UserEventHandler (IO Bool) widget Impure event = widget -> IO (Bool, event)
  UserEventHandler (a -> b)  widget purity event = a -> UserEventHandler b widget purity event

-- | Internal class for converting user event handlers to encoded 'EventHandler' values.
class ToEventHandler gtkEventHandler widget purity where
  -- | Convert from a user event handler to an 'EventHandler'.
  toEventHandler
    :: UserEventHandler gtkEventHandler widget purity event
    -> EventHandler gtkEventHandler widget purity event

instance ToEventHandler (IO ()) widget Pure where
  toEventHandler = PureEventHandler . OnlyEvent . pure

instance ToEventHandler (IO Bool) widget Pure where
  toEventHandler = PureEventHandler . ReturnAndEvent . pure

instance ToEventHandler (IO ()) widget Impure where
  toEventHandler eh = ImpureEventHandler (OnlyEvent . eh)

instance ToEventHandler (IO Bool) widget Impure where
  toEventHandler eh = ImpureEventHandler (ReturnAndEvent . eh)

instance (ToEventHandler b widget purity) => ToEventHandler (a -> b) widget purity where
  toEventHandler f = EventHandlerFunction (toEventHandler . f)
