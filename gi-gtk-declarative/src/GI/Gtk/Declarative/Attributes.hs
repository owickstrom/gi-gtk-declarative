{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Attribute lists on declarative objects, supporting the underlying
-- attributes from "Data.GI.Base.Attributes", along with CSS class lists, and
-- pure and impure event EventHandlers.

module GI.Gtk.Declarative.Attributes
  ( Attribute(..)
  , classes
  , ClassSet
  -- * Custom Attributes
  , customAttribute
  , createCustomAttributes
  , patchCustomAttributes
  , destroyCustomAttributes
  , subscribeCustomAttributes
  -- * Event Handling
  , on
  , onM
  -- * EventHandlers
  , EventHandler(..)
  )
where

import           Control.Monad                  ( forM, forM_ )
import           Data.Foldable                  ( fold )
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

import           GI.Gtk.Declarative.Attributes.Custom
import           GI.Gtk.Declarative.Attributes.Internal.Conversions
import           GI.Gtk.Declarative.Attributes.Internal.EventHandler
import           GI.Gtk.Declarative.EventSource.Subscription

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
    :: CustomAttributeDecl widget event
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

-- move the state data into "Collected" ? move these functions to a different module?

-- | Create a custom attribute from its declarative form
customAttribute
  :: CustomAttribute widget decl
  => decl event
  -> Attribute widget event
customAttribute decl =
  Custom $ CustomAttributeDecl decl

createCustomAttributes
  :: widget
  -> Vector (Attribute widget event)
  -> IO (Vector (CustomAttributeState widget))
createCustomAttributes widget attrs =
  forM (filterToCustom attrs) $ \(CustomAttributeDecl attr) -> do
    CustomAttributeState <$> attrCreate widget attr

patchCustomAttributes
  :: forall widget e1 e2
   . widget
  -> Vector (CustomAttributeState widget)
  -> Vector (Attribute widget e1)
  -> Vector (Attribute widget e2)
  -> IO (Vector (CustomAttributeState widget))
patchCustomAttributes widget oldStates oldDecls newDecls =
  Vector.mapMaybe id <$> Vector.generateM maxLength patchIndex
  where
    maxLength = maximum [Vector.length oldStates, Vector.length oldDecls', Vector.length newDecls']
    oldDecls' = filterToCustom oldDecls
    newDecls' = filterToCustom newDecls
    
    patchIndex i =
      case (oldStates !? i, oldDecls' !? i, newDecls' !? i) of
        (Just oldState, Just oldDecl, Just newDecl) ->
          Just <$> patchAttribute oldState oldDecl newDecl
        (Just oldState, Just oldDecl, Nothing) -> do
          putStrLn "attr removed"
          Nothing <$ withCustomAttribute attrDestroy widget oldState oldDecl
        (Nothing, Nothing, Just (CustomAttributeDecl attr)) ->
          Just . CustomAttributeState <$> attrCreate widget attr
        _ ->
          error "state/decl mismatch: this means there is a bug in gi-gtk-declarative"
    
    patchAttribute
      :: CustomAttributeState widget
      -> CustomAttributeDecl widget e1
      -> CustomAttributeDecl widget e2
      -> IO (CustomAttributeState widget)
    patchAttribute (CustomAttributeState (oldState :: State d1))
                   (CustomAttributeDecl (oldDecl :: d2 e1))
                   (CustomAttributeDecl (newDecl :: d3 e2)) =
      case (eqT @d1 @d2, eqT @d1 @d3) of
        (Just Refl, Just Refl) -> do
          -- the new attribute has the same type as the old one, so we can patch
          CustomAttributeState <$> attrPatch widget oldState oldDecl newDecl
        (Just Refl, Nothing) -> do
          -- the new attribute has a different type to the old one, so we need to destroy and recreate
          attrDestroy widget oldState oldDecl
          CustomAttributeState <$> attrCreate widget newDecl
        _ ->
          error "state/decl mismatch: this means there is a bug in gi-gtk-declarative"

destroyCustomAttributes
  :: widget
  -> Vector (CustomAttributeState widget)
  -> Vector (Attribute widget event)
  -> IO ()
destroyCustomAttributes widget states attrs = do
  sequence_ $ Vector.zipWith
    (withCustomAttribute attrDestroy widget)
    states
    (filterToCustom attrs)

subscribeCustomAttributes
  :: widget
  -> Vector (CustomAttributeState widget)
  -> Vector (Attribute widget event)
  -> (event -> IO ())
  -> IO Subscription
subscribeCustomAttributes widget states attrs cb =
  fold $ Vector.zipWith
    (withCustomAttribute (\w s d -> attrSubscribe w s d cb) widget)
    states
    (filterToCustom attrs)

filterToCustom
  :: Vector (Attribute widget event)
  -> Vector (CustomAttributeDecl widget event)
filterToCustom = Vector.mapMaybe
  (\case
      Custom a -> Just a
      _        -> Nothing
  )

withCustomAttribute
  :: (forall decl. CustomAttribute widget decl => widget -> State decl -> decl event -> b)
  -> widget
  -> CustomAttributeState widget
  -> CustomAttributeDecl widget event
  -> b
withCustomAttribute cb
                    widget
                    (CustomAttributeState (state :: State d1))
                    (CustomAttributeDecl (decl :: d2 event)) =
  case eqT @d1 @d2 of
    Just Refl ->
      cb widget state decl
    _ ->
      error "state/decl mismatch: this means there is a bug in gi-gtk-declarative"