{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Custom attributes that have arbitrary state and patching behaviour, yet
-- can be attached to declarative widgets just like normal Gtk+ attributes.
module GI.Gtk.Declarative.Attributes.Custom
  ( CustomAttribute (..)
  , CustomAttributeDecl (..)
  , CustomAttributeState (..)
  , createCustomAttributes
  , patchCustomAttributes
  , destroyCustomAttributes
  , subscribeCustomAttributes
  ) where

import           Control.Monad                               (forM)
import           Data.Foldable                               (fold)
import           Data.Typeable
import           Data.Vector                                 (Vector, (!?))
import qualified Data.Vector                                 as Vector

import           GI.Gtk.Declarative.EventSource.Subscription

-- | The declarative form of a custom attribute. The actual type of of the
-- attribute is hidden here: only the widget and event types are exposed.
data CustomAttributeDecl widget event where
  CustomAttributeDecl
    :: CustomAttribute widget decl
    => decl event
    -> CustomAttributeDecl widget event

deriving instance Functor (CustomAttributeDecl widget)

-- | The runtime state for a custom attribute. The actual type of
-- the attribute is hidden here: only the widget type is exposed.
data CustomAttributeState widget where
  CustomAttributeState
    :: CustomAttribute widget decl
    => State decl
    -> CustomAttributeState widget

-- | Defines types that can be used as declarative custom attributes on the given widget type.
class (Typeable decl, Typeable (State decl), Functor decl) => CustomAttribute widget decl where

  -- | The runtime state of this attribute. This is preserved
  -- between calls to attrCreate/attrPatch/attrDestroy.
  data State decl

  -- | Called when this attribute is first attached to a widget in the tree
  attrCreate :: widget -> decl event -> IO (State decl)

  -- | Called when the widget tree is being patched.
  attrPatch :: widget -> State decl -> decl event1 -> decl event2 -> IO (State decl)

  -- | Called when the associated widget is removed from the widget tree.
  attrDestroy :: widget -> State decl -> decl event -> IO ()

  -- | Attach event handlers to this attribute.
  attrSubscribe :: widget -> State decl -> decl event -> (event -> IO ()) -> IO Subscription

-- | Runs the create action for each custom attribute.
createCustomAttributes
  :: widget
  -> Vector (CustomAttributeDecl widget event)
  -> IO (Vector (CustomAttributeState widget))
createCustomAttributes widget attrs =
  forM attrs $ \(CustomAttributeDecl attr) -> do
    CustomAttributeState <$> attrCreate widget attr

stateDeclMismatchError :: a
stateDeclMismatchError =
  error "state/decl mismatch: this means there is a bug in gi-gtk-declarative"

-- | Patches custom attribute state so it matches the latest declarative attributes.
patchCustomAttributes
  :: forall widget e1 e2
   . widget
  -> Vector (CustomAttributeState widget)
  -> Vector (CustomAttributeDecl widget e1)
  -> Vector (CustomAttributeDecl widget e2)
  -> IO (Vector (CustomAttributeState widget))
patchCustomAttributes widget oldStates oldDecls newDecls =
  Vector.mapMaybe id <$> Vector.generateM maxLength patchIndex
 where
  maxLength = maximum
    [Vector.length oldStates, Vector.length oldDecls, Vector.length newDecls]

  patchIndex i = case (oldStates !? i, oldDecls !? i, newDecls !? i) of
    (Just oldState, Just oldDecl, Just newDecl) ->
      Just <$> patchAttribute oldState oldDecl newDecl
    (Just oldState, Just oldDecl, Nothing) -> do
      Nothing <$ withCustomAttribute attrDestroy widget oldState oldDecl
    (Nothing, Nothing, Just (CustomAttributeDecl attr)) ->
      Just . CustomAttributeState <$> attrCreate widget attr
    _ -> stateDeclMismatchError

  patchAttribute
    :: CustomAttributeState widget
    -> CustomAttributeDecl widget e1
    -> CustomAttributeDecl widget e2
    -> IO (CustomAttributeState widget)
  patchAttribute (CustomAttributeState (oldState :: State d1))
                 (CustomAttributeDecl (oldDecl :: d2 e1))
                 (CustomAttributeDecl (newDecl :: d3 e2))
    = case (eqT @d1 @d2, eqT @d1 @d3) of
      (Just Refl, Just Refl) -> do
        -- the new attribute has the same type as the old one, so we can patch
        CustomAttributeState <$> attrPatch widget oldState oldDecl newDecl
      (Just Refl, Nothing) -> do
        -- the new attribute has a different type to the old one, so we need to destroy and recreate
        attrDestroy widget oldState oldDecl
        CustomAttributeState <$> attrCreate widget newDecl
      _ -> stateDeclMismatchError

-- | Runs the destroy action for the given custom attributes.
destroyCustomAttributes
  :: widget
  -> Vector (CustomAttributeState widget)
  -> Vector (CustomAttributeDecl widget event)
  -> IO ()
destroyCustomAttributes widget states attrs = do
  fold $ Vector.zipWith (withCustomAttribute attrDestroy widget) states attrs

-- | Attaches event listeners to already-created custom attributes.
subscribeCustomAttributes
  :: widget
  -> Vector (CustomAttributeState widget)
  -> Vector (CustomAttributeDecl widget event)
  -> (event -> IO ())
  -> IO Subscription
subscribeCustomAttributes widget states attrs cb = fold $ Vector.zipWith
  (withCustomAttribute (\w s d -> attrSubscribe w s d cb) widget)
  states
  attrs

-- | Call a function with a declarative custom attribute and its associated state,
-- which must be of the correct type.
withCustomAttribute
  :: (  forall decl
      . CustomAttribute widget decl
     => widget
     -> State decl
     -> decl event
     -> b
     )
  -> widget
  -> CustomAttributeState widget
  -> CustomAttributeDecl widget event
  -> b
withCustomAttribute cb
                    widget
                    (CustomAttributeState (state :: State d1))
                    (CustomAttributeDecl (decl :: d2 event))
  = case eqT @d1 @d2 of
    Just Refl -> cb widget state decl
    _         -> stateDeclMismatchError
