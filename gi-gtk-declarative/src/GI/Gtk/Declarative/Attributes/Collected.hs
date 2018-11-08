{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

-- | Internal helpers for applying attributes and signal handlers to GTK+
-- widgets.
module GI.Gtk.Declarative.Attributes.Collected
  ( CollectedProperties
  , Collected(..)
  , collectAttributes
  , constructProperties
  , updateProperties
  , updateClasses
  )
where

import           Data.Foldable
import qualified Data.Text                     as Text
import           Data.Text                                ( Text )
import qualified Data.HashSet                  as HashSet
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashMap.Strict                      ( HashMap )
import qualified Data.GI.Base.Attributes       as GI
import qualified GI.Gtk                        as Gtk
import           Data.Typeable
import           GHC.TypeLits
import           Data.Vector                              ( Vector )

import           GI.Gtk.Declarative.Attributes

data CollectedProperty widget where
  CollectedProperty
    :: (GI.AttrOpAllowed 'GI.AttrConstruct info widget
      , GI.AttrOpAllowed 'GI.AttrSet info widget
      , GI.AttrGetC info widget attr getValue
      , GI.AttrSetTypeConstraint info setValue
      , KnownSymbol attr
      , Typeable attr
      , Eq setValue
      , Typeable setValue
      )
   => GI.AttrLabelProxy attr -> setValue -> CollectedProperty widget

type CollectedProperties widget = HashMap Text (CollectedProperty widget)

data Collected widget event = Collected
  { collectedClasses :: ClassSet
  , collectedProperties :: CollectedProperties widget
  }

instance Semigroup (Collected widget event) where
  c1 <> c2 =
    Collected
      (collectedClasses c1 <> collectedClasses c2)
      (collectedProperties c1 <> collectedProperties c2)

instance Monoid (Collected widget event) where
  mempty = Collected mempty mempty

collectAttributes :: Vector (Attribute widget event) -> Collected widget event
collectAttributes = foldl' go mempty
 where
  go
    :: Collected widget event
    -> Attribute widget event
    -> Collected widget event
  go Collected {..} = \case
    attr := value -> Collected
      { collectedProperties = HashMap.insert (Text.pack (symbolVal attr))
                                             (CollectedProperty attr value)
                                             collectedProperties
      , ..
      }
    Classes classSet ->
      Collected {collectedClasses = collectedClasses <> classSet, ..}
    _ -> Collected {..}

constructProperties
  :: Collected widget event -> [GI.AttrOp widget 'GI.AttrConstruct]
constructProperties c = map
  (\(CollectedProperty attr value) -> attr Gtk.:= value)
  (HashMap.elems (collectedProperties c))

updateProperties
  :: widget -> CollectedProperties widget -> CollectedProperties widget -> IO ()
updateProperties (widget' :: widget) oldProps newProps = do
  let toAdd  = HashMap.elems (HashMap.difference newProps oldProps)
      setOps = mconcat
        (HashMap.elems (HashMap.intersectionWith toMaybeSetOp oldProps newProps)
        )
  GI.set widget' (map (toSetOp (Proxy @widget)) toAdd <> setOps)
 where
  toSetOp
    :: Proxy widget
    -> CollectedProperty widget
    -> Gtk.AttrOp widget 'GI.AttrSet
  toSetOp _ (CollectedProperty attr value) = attr Gtk.:= value

  toMaybeSetOp
    :: CollectedProperty widget
    -> CollectedProperty widget
    -> [Gtk.AttrOp widget 'GI.AttrSet]
  toMaybeSetOp (CollectedProperty attr (v1 :: t1)) (CollectedProperty _ (v2 :: t2))
    = case eqT @t1 @t2 of
      Just Refl | v1 /= v2 -> pure (attr Gtk.:= v2)
      _                    -> mempty

updateClasses :: Gtk.StyleContext -> ClassSet -> ClassSet -> IO ()
updateClasses sc old new = do
  let toAdd    = HashSet.difference new old
      toRemove = HashSet.difference old new
  mapM_ (Gtk.styleContextAddClass sc)    toAdd
  mapM_ (Gtk.styleContextRemoveClass sc) toRemove
