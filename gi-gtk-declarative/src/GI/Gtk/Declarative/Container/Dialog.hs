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

module GI.Gtk.Declarative.Container.Dialog
  ( Dialog
  , dialog
  , DialogChild
  , dialogContent
  , dialogButton
  )
where

import           Control.Monad                          (forM_, void)
import           Data.Either                            (partitionEithers)
import           Data.Function                          ((&))
import           Data.Text                              (Text)
import           Data.Typeable
import qualified GI.GObject                             as GI
import qualified GI.Gtk                                 as Gtk

import           GI.Gtk.Declarative.Attributes
import           GI.Gtk.Declarative.Attributes.Internal
import           GI.Gtk.Declarative.Children
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Markup
import           GI.Gtk.Declarative.Patch

data Dialog event where
  Dialog
    :: [Attribute Gtk.Dialog event]
    -> (Children DialogChild event)
    -> Dialog event

dialogSplitChildren :: Dialog event -> ([Widget event], [(Text, event)])
dialogSplitChildren (Dialog _ children) =
  unChildren children
  & map (\case
    DialogContent w -> Left w
    DialogButton label event -> Right (label, event))
  & partitionEithers

dialogContentChildren :: Dialog event -> [Widget event]
dialogContentChildren = fst . dialogSplitChildren

instance Patchable Dialog where
  create (Dialog attrs children) = do
    let attrOps = concatMap extractAttrConstructOps attrs
    widget' <- Gtk.new Gtk.Dialog attrOps
    contentArea <- Gtk.dialogGetContentArea widget'
    sc <- Gtk.widgetGetStyleContext widget'
    mapM_ (addClass sc) attrs
    forM_ (unChildren children) $ \case
      DialogContent child -> do
        childWidget <- create child
        #add contentArea childWidget
      DialogButton label event -> do
        void (#addButton widget' label 0)
    Gtk.toWidget widget'
  patch b1 b2 = undefined

instance EventSource Dialog where
  subscribe d@(Dialog attrs children) widget cb = do
    dialog <- Gtk.unsafeCastTo Gtk.Dialog widget
    contentArea <- Gtk.dialogGetContentArea dialog
    contentAreaWidgets <- Gtk.containerGetChildren contentArea
    let (contentChildren, buttons) = dialogSplitChildren d
    flip foldMap (zip contentChildren contentAreaWidgets) $
      (\(c, w) -> subscribe c w cb)
    actionArea <-
      Gtk.dialogGetActionArea dialog >>= Gtk.unsafeCastTo Gtk.Container
    actionAreaWidgets <- Gtk.containerGetChildren actionArea
    flip foldMap (zip buttons actionAreaWidgets) $
      (\((label, event), w) -> do
         b <- Gtk.unsafeCastTo Gtk.Button w
         sid <- Gtk.on b #clicked (cb event)
         return (fromCancellation (GI.signalHandlerDisconnect w sid)))

instance Functor Dialog where
  fmap f (Dialog attrs children) =
    Dialog (fmap f <$> attrs) (f <$> children)

dialog
  :: ( Typeable event
     )
  => [Attribute Gtk.Dialog event]
  -> MarkupOf DialogChild event ()
  -> Widget event
dialog attrs = Widget . Dialog attrs . toChildren

data DialogChild event where
  DialogContent
    :: Widget event
    -> DialogChild event
  DialogButton
    :: Text
    -> event
    -> DialogChild event

instance Functor DialogChild where
  fmap f = \case
    DialogContent child -> DialogContent (f <$> child)
    DialogButton label event -> DialogButton label (f event)

instance EventSource DialogChild where
  subscribe (DialogContent w) = subscribe w

dialogContent
  :: (Typeable event)
  => Widget event
  -> MarkupOf DialogChild event ()
dialogContent = single . DialogContent

dialogButton
  :: (Typeable event)
  => Text
  -> event
  -> MarkupOf DialogChild event ()
dialogButton t = single . DialogButton t
