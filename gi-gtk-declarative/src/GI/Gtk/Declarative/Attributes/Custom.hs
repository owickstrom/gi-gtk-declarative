{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Custom attributes that have arbitrary state and patching behaviour, yet
-- can be attached to declarative widgets just like normal Gtk+ attributes.
module GI.Gtk.Declarative.Attributes.Custom
  ( CustomAttribute (..)
  , CustomAttributeKey (..)
  , CustomAttributeDecl (..)
  , CustomAttributeState (..)
  , CollectedCustomAttributeStates
  , createCustomAttributes
  , patchCustomAttributes
  , destroyCustomAttributes
  , subscribeCustomAttributes
  ) where

import           Control.Monad                               (forM, forM_)
import           Data.Foldable                               (traverse_)
import           Data.Hashable                               (Hashable (..))
import           Data.HashMap.Strict                         (HashMap)
import qualified Data.HashMap.Strict                         as HashMap
import           Data.Typeable
import           Data.Vector                                 (Vector)
import qualified Data.Vector                                 as Vector

import           GI.Gtk.Declarative.EventSource.Subscription

-- | The declarative form of a custom attribute. The actual type of of the
-- attribute is hidden here: only the widget and event types are exposed.
data CustomAttributeDecl widget event where
  CustomAttributeDecl
    :: CustomAttribute widget decl
    => CustomAttributeKey
    -> decl event
    -> CustomAttributeDecl widget event

deriving instance Functor (CustomAttributeDecl widget)

-- | The runtime state for a custom attribute. The actual type of
-- the attribute is hidden here: only the widget type is exposed.
data CustomAttributeState widget where
  CustomAttributeState
    :: CustomAttribute widget decl
    => AttrState decl
    -> CustomAttributeState widget

-- | This is the state that is maintained for the custom attributes on
-- a particular widget. It is grouped by key for the convenience of
-- the diffing algorithm.
type CollectedCustomAttributeStates widget =
  HashMap CustomAttributeKey [CustomAttributeState widget]

-- | The declarative form of a custom attribute, plus the runtime state.
data CustomAttributeDeclState widget event where
  CustomAttributeDeclState
    :: CustomAttribute widget decl
    => CustomAttributeKey
    -> decl event
    -> AttrState decl
    -> CustomAttributeDeclState widget event

-- | A key for a custom attribute. The actual type of the key is hidden:
-- we only care that it is typeable and hashable.
data CustomAttributeKey where
  CustomAttributeKey
    :: (Typeable key, Eq key, Hashable key)
    => key
    -> CustomAttributeKey

instance Eq CustomAttributeKey where
  CustomAttributeKey (k1 :: k1) == CustomAttributeKey (k2 :: k2) =
    case eqT @k1 @k2 of
      Just Refl -> k1 == k2
      Nothing   -> False

instance Hashable CustomAttributeKey where
  hashWithSalt salt (CustomAttributeKey k) =
    hashWithSalt salt k

-- | Defines types that can be used as declarative custom attributes on the given widget type.
class (Typeable decl, Typeable (AttrState decl), Functor decl) => CustomAttribute widget decl where

  -- | The runtime state of this attribute. This is preserved
  -- between calls to attrCreate/attrPatch/attrDestroy.
  data AttrState decl

  -- | Called when this attribute is first attached to a widget in the tree
  attrCreate :: widget -> decl event -> IO (AttrState decl)

  -- | Called when the widget tree is being patched.
  attrPatch :: widget -> AttrState decl -> decl event1 -> decl event2 -> IO (AttrState decl)

  -- | Called when the associated widget is removed from the widget tree.
  attrDestroy :: widget -> AttrState decl -> decl event -> IO ()
  attrDestroy _widget _state _decl =
    pure ()

  -- | Attach event handlers to this attribute.
  attrSubscribe :: widget -> AttrState decl -> decl event -> (event -> IO ()) -> IO Subscription
  attrSubscribe _widget _state _decl _cb =
    mempty

-- | Runs the create action for each custom attribute.
createCustomAttributes
  :: widget
  -> Vector (CustomAttributeDecl widget event)
  -> IO (CollectedCustomAttributeStates widget)
createCustomAttributes widget attrs =
  flip foldMap attrs $ \(CustomAttributeDecl key attr) -> do
    state <- attrCreate widget attr
    pure (HashMap.singleton key [CustomAttributeState state])

-- | Patches custom attribute state so it matches the latest declarative attributes.
patchCustomAttributes
  :: forall widget e1 e2
   . widget
  -> CollectedCustomAttributeStates widget
  -> Vector (CustomAttributeDecl widget e1)
  -> Vector (CustomAttributeDecl widget e2)
  -> IO (CollectedCustomAttributeStates widget)
patchCustomAttributes widget oldStates oldDecls newDecls = do

  -- destroy attributes under keys that no longer exist
  forM_ removedKeys $ \cads ->
    forM_ cads $ \(CustomAttributeDeclState _key decl state) ->
      attrDestroy widget state decl

  -- patch attributes under keys that have been neither added nor removed
  preservedStates <- forM preservedKeys $ \(old, new) ->
    zipCreateUpdateDeleteM old new
      (\(CustomAttributeDecl _k decl) ->
        CustomAttributeState <$> attrCreate widget decl
      )
      (\(CustomAttributeDeclState _k1 (d1 :: d1 e1) state) (CustomAttributeDecl _k2 (d2 :: d2 e2)) ->
        case eqT @d1 @d2 of
          Just Refl ->
            CustomAttributeState <$> attrPatch widget state d1 d2
          Nothing -> do
            attrDestroy widget state d1
            CustomAttributeState <$> attrCreate widget d2
      )
      (\(CustomAttributeDeclState _k decl state) ->
        attrDestroy widget state decl
      )

  -- create attributes under newly added keys
  addedStates <- forM addedKeys $ \decls ->
    forM decls $ \(CustomAttributeDecl _key decl) ->
      CustomAttributeState <$> attrCreate widget decl

  pure (preservedStates <> addedStates)

  where
    oldKeys :: HashMap CustomAttributeKey [CustomAttributeDeclState widget e1]
    oldKeys = combineDeclStates oldDecls oldStates

    newKeys :: HashMap CustomAttributeKey [CustomAttributeDecl widget e2]
    newKeys = HashMap.fromListWith (flip (<>))
      [(key, [d]) | d@(CustomAttributeDecl key _) <- Vector.toList newDecls]

    removedKeys :: HashMap CustomAttributeKey [CustomAttributeDeclState widget e1]
    removedKeys = HashMap.difference oldKeys newKeys

    addedKeys :: HashMap CustomAttributeKey [CustomAttributeDecl widget e2]
    addedKeys = HashMap.difference newKeys oldKeys

    preservedKeys :: HashMap CustomAttributeKey ([CustomAttributeDeclState widget e1], [CustomAttributeDecl widget e2])
    preservedKeys = HashMap.intersectionWith (,) oldKeys newKeys

-- | Runs the destroy action for the given custom attributes.
destroyCustomAttributes
  :: widget
  -> CollectedCustomAttributeStates widget
  -> Vector (CustomAttributeDecl widget event)
  -> IO ()
destroyCustomAttributes widget states decls =
  forM_ (combineDeclStates decls states) $ \dss ->
    forM_ dss $ \(CustomAttributeDeclState _key decl state) ->
      attrDestroy widget state decl

-- | Attaches event listeners to already-created custom attributes.
subscribeCustomAttributes
  :: widget
  -> CollectedCustomAttributeStates widget
  -> Vector (CustomAttributeDecl widget event)
  -> (event -> IO ())
  -> IO Subscription
subscribeCustomAttributes widget states decls cb =
  flip foldMap (combineDeclStates decls states) $ \dss ->
    flip foldMap dss $ \(CustomAttributeDeclState _key decl state) ->
      attrSubscribe widget state decl cb

combineDeclStates
  :: forall widget event
   . Vector (CustomAttributeDecl widget event)
  -> CollectedCustomAttributeStates widget
  -> HashMap CustomAttributeKey [CustomAttributeDeclState widget event]
combineDeclStates decls =
  HashMap.intersectionWith (zipWith combine) declMap
  where
    declMap :: HashMap CustomAttributeKey [CustomAttributeDecl widget event]
    declMap = HashMap.fromListWith (flip (<>))
      [(key, [d]) | d@(CustomAttributeDecl key _) <- Vector.toList decls]

    combine
      :: CustomAttributeDecl widget event
      -> CustomAttributeState widget
      -> CustomAttributeDeclState widget event
    combine (CustomAttributeDecl key (decl :: d1 event))
            (CustomAttributeState (state :: AttrState d2)) =
      case eqT @d1 @d2 of
        Just Refl -> CustomAttributeDeclState key decl state
        Nothing ->
          error "state/decl mismatch: this means there is a bug in gi-gtk-declarative"

-- | A special zip that calls different zip functions depending on whether the list index
-- exists only in the second list (create), in both lists (update) or only in the first
-- list (delete).
zipCreateUpdateDeleteM
  :: Applicative m
  => [a]
  -> [b]
  -> (b -> m c)       -- ^ Create
  -> (a -> b -> m c)  -- ^ Update
  -> (a -> m ())      -- ^ Delete
  -> m [c]
zipCreateUpdateDeleteM as bs c u d =
  case (as, bs) of
    (a : as', b : bs') -> (:) <$> u a b <*> zipCreateUpdateDeleteM as' bs' c u d
    ([], bs')          -> traverse c bs'
    (as', [])          -> [] <$ traverse_ d as'
