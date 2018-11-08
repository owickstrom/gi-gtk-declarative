{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

-- | Implementation of 'Gtk.Box' as a declarative container.
module GI.Gtk.Declarative.Container.Box
  ( BoxChild(..)
  , boxChild
  )
where

import           Data.Word                      (Word32)

import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Markup

-- | Described a child widget to be added with 'boxAppend' to a 'Box'.
data BoxChild event = BoxChild
  { expand  :: Bool
  , fill    :: Bool
  , padding :: Word32
  , child   :: Widget event
  }
  deriving (Functor)

-- | Construct a box child with the given 'boxAppend' parameters.
boxChild
  :: Bool
  -> Bool
  -> Word32
  -> Widget event
  -> MarkupOf BoxChild event ()
boxChild expand fill padding child = single BoxChild {..}

instance Patchable BoxChild where
  create = create . child
  patch s b1 b2 = patch s (child b1) (child b2)

instance EventSource BoxChild where
  subscribe BoxChild{..} = subscribe child
