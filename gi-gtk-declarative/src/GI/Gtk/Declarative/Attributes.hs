{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Attribute lists on declarative objects, supporting the underlying
-- attributes from "Data.GI.Base.Attributes", along with CSS class lists, and
-- pure and impure event EventHandlers.

module GI.Gtk.Declarative.Attributes
  ( Attribute(..)
  , classes
  , ClassSet
  , createCustomAttributes
  , patchCustomAttributes
  , destroyCustomAttributes
  -- * Event Handling
  , on
  , onM
  -- * EventHandlers
  , EventHandler(..)
  )
where

import Control.Monad (forM)
import qualified Data.Dynamic as Dynamic
import           Data.Dynamic (Dynamic)
import qualified Data.GI.Base.Attributes       as GI
import qualified Data.GI.Base.Signals          as GI
import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet                  as HashSet
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Typeable
import qualified Data.Vector                   as Vector
import           Data.Vector                    ( Vector, (!?) )
import           GHC.TypeLits                   ( KnownSymbol
                                                , Symbol
                                                )
import qualified GI.Gtk                        as Gtk

import           GI.Gtk.Declarative.Attributes.Internal.EventHandler
import           GI.Gtk.Declarative.Attributes.Internal.Conversions

import GI.Gtk.Declarative.Attributes.Custom

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
    ::(GI.AttrOpAllowed 'GI.AttrConstruct info widget
      , GI.AttrOpAllowed 'GI.AttrSet info widget
      , GI.AttrGetC info widget attr getValue
      , GI.AttrSetTypeConstraint info setValue
      , KnownSymbol attr
      , Typeable attr
      , Eq setValue
      , Typeable setValue
      )
   => GI.AttrLabelProxy (attr :: Symbol) -> setValue -> Attribute widget event
  -- | Defines a set of CSS classes for the underlying widget's style context.
  -- Use the 'classes' function instead of this constructor directly.
  Classes
    ::Gtk.IsWidget widget
    => ClassSet
    -> Attribute widget event
  -- | Emit events using a pure event handler. Use the 'on' function, instead of this
  -- constructor directly.
  OnSignalPure
    ::( Gtk.GObject widget
       , GI.SignalInfo info
       , gtkCallback ~ GI.HaskellCallbackType info
       , ToGtkCallback gtkCallback Pure
       )
    => Gtk.SignalProxy widget info
    -> EventHandler gtkCallback widget Pure event
    -> Attribute widget event
  -- | Emit events using a pure event handler. Use the 'on' function, instead of this
  -- constructor directly.
  OnSignalImpure
    ::( Gtk.GObject widget
       , GI.SignalInfo info
       , gtkCallback ~ GI.HaskellCallbackType info
       , ToGtkCallback gtkCallback Impure
       )
    => Gtk.SignalProxy widget info
    -> EventHandler gtkCallback widget Impure event
    -> Attribute widget event
  Custom
    ::( Gtk.IsWidget widget
      , Typeable widget
      , Typeable internalState
      )
    => CustomAttribute widget internalState event
    -> Attribute widget event

-- | A set of CSS classes.
type ClassSet = HashSet Text

-- | Attributes have a 'Functor' instance that maps events in all
-- event handler.
instance Functor (Attribute widget) where
  fmap f = \case
    attr := value            -> attr := value
    Classes cs               -> Classes cs
    OnSignalPure   signal eh -> OnSignalPure signal (fmap f eh)
    OnSignalImpure signal eh -> OnSignalImpure signal (fmap f eh)
    Custom attr              -> Custom (fmap f attr)

-- | Define the CSS classes for the underlying widget's style context. For these
-- classes to have any effect, this requires a 'Gtk.CssProvider' with CSS files
-- loaded, to be added to the GDK screen. You probably want to do this in your
-- entry point when setting up GTK.
classes :: Gtk.IsWidget widget => [T.Text] -> Attribute widget event
classes = Classes . HashSet.fromList

-- | Emit events, using a pure event handler, by subcribing to the specified
-- signal.
on
  :: ( Gtk.GObject widget
     , GI.SignalInfo info
     , gtkCallback ~ GI.HaskellCallbackType info
     , ToGtkCallback gtkCallback Pure
     , ToEventHandler gtkCallback widget Pure
     , userEventHandler ~ UserEventHandler gtkCallback widget Pure event
     )
  => Gtk.SignalProxy widget info
  -> userEventHandler
  -> Attribute widget event
on signal = OnSignalPure signal . toEventHandler

-- | Emit events, using an impure event handler receiving the 'widget' and returning
-- an 'IO' action of 'event', by subcribing to the specified signal.
onM
  :: ( Gtk.GObject widget
     , GI.SignalInfo info
     , gtkCallback ~ GI.HaskellCallbackType info
     , ToGtkCallback gtkCallback Impure
     , ToEventHandler gtkCallback widget Impure
     , userEventHandler ~ UserEventHandler gtkCallback widget Impure event
     )
  => Gtk.SignalProxy widget info
  -> userEventHandler
  -> Attribute widget event
onM signal = OnSignalImpure signal . toEventHandler

-- move the state data into "Collected" ?

createCustomAttributes :: widget -> Vector (Attribute widget event) -> IO (Vector Dynamic)
createCustomAttributes widget attrs = do
  forM (filterToCustom attrs) $ \(DynCustomAttribute a) -> do
    Dynamic.toDyn <$> attrCreate a widget

patchCustomAttributes :: widget -> Vector Dynamic -> Vector (Attribute widget event) -> IO (Vector Dynamic)
patchCustomAttributes widget oldStates newAttrs = do
  Vector.mapMaybe id <$> Vector.generateM maxLength patchIndex
  where
    maxLength = max (Vector.length oldStates) (Vector.length newAttrs')
    newAttrs' = filterToCustom newAttrs
    patchIndex :: Int -> IO (Maybe Dynamic)
    patchIndex i =
      case (oldStates !? i, newAttrs' !? i) of
        -- old and new attributes have the same type, so we can just patch
        (Just oldState, Just (DynCustomAttribute a)) | Just s <- Dynamic.fromDynamic oldState ->
         Just . Dynamic.toDyn <$> attrPatch a widget s

        -- old and new attributes have different types: must destroy and recreate
        (Just oldState, Just (DynCustomAttribute a)) | otherwise -> do
          error "delete old, create new"
        
        -- old attribute doesn't exist in new world, so destroy it
        (Just oldState, Nothing) ->
          error "delete old"
        
        -- a new attribute needs creating
        (Nothing, Just (DynCustomAttribute a)) ->
          Just . Dynamic.toDyn <$> attrCreate a widget
        
        (Nothing, Nothing) ->
          error "this should not happen"

destroyCustomAttributes :: widget -> Vector Dynamic -> Vector (Attribute widget event) -> IO ()
destroyCustomAttributes widget states attrs =
  sequence_ (Vector.zipWith f states (filterToCustom attrs))
  where
    f state (DynCustomAttribute a) =
      case (Dynamic.fromDynamic state) of
        Just s -> attrDestroy a widget s
        Nothing -> error "Custom attribute state mismatch"

filterToCustom :: Vector (Attribute widget event) -> Vector (DynCustomAttribute widget event)
filterToCustom = Vector.mapMaybe
  (\case
      Custom a -> Just (DynCustomAttribute a)
      _        -> Nothing
  )

data DynCustomAttribute widget event where
  DynCustomAttribute
    :: Typeable internalState
    => CustomAttribute widget internalState event
    -> DynCustomAttribute widget event