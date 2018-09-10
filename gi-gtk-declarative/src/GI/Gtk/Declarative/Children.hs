{-# LANGUAGE DeriveFunctor          #-}
module GI.Gtk.Declarative.Children where

import           GI.Gtk.Declarative.Markup

newtype Children child event = Children { unChildren :: [child event] }
  deriving (Functor)

toChildren :: MarkupOf child event () -> Children child event
toChildren = Children . runMarkup
