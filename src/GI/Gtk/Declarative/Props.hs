{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

-- | Property lists on declarative objects, supporting the underlying
-- attributes from "Data.GI.Base.Attributes".

module GI.Gtk.Declarative.Props
  ( PropPair(..)
  , classes
  ) where

import qualified Data.GI.Base.Attributes as GI
import qualified Data.HashSet            as HashSet
import           Data.Text               (Text)
import           Data.Typeable
import           GHC.TypeLits            (Symbol)
import           GHC.TypeLits            (KnownSymbol, symbolVal)
import qualified GI.Gtk                  as Gtk

import           GI.Gtk.Declarative.CSS

infix 5 :=

data PropPair w where
  (:=)
    :: (GI.AttrGetC info w attr value
      , GI.AttrOpAllowed 'GI.AttrConstruct info w
      , GI.AttrOpAllowed 'GI.AttrSet info w
      , GI.AttrSetTypeConstraint info value
      , KnownSymbol attr
      , Typeable attr
      , Eq value
      )
    =>  GI.AttrLabelProxy (attr :: Symbol) -> value -> PropPair w
  Classes
    :: Gtk.IsWidget w
    => ClassSet
    -> PropPair w

classes :: Gtk.IsWidget w => [Text] -> PropPair w
classes = Classes . HashSet.fromList

instance Eq (PropPair w) where
  ((_ :: GI.AttrLabelProxy attr1) := v1) == ((_ :: GI.AttrLabelProxy attr2) := v2) =
    case eqT @attr1 @attr2 of
      Just Refl -> v1 == v2
      Nothing   -> False
  Classes c1 == Classes c2 = c1 == c2
  _ == _ = False

instance Show a => Show (PropPair a) where
  show (attr := _value) = symbolVal attr <> " := " <> "???"
  show (Classes cs)     = "Classes " <> show cs
