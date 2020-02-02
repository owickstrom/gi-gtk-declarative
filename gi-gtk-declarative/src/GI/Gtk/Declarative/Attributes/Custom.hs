{-# LANGUAGE DeriveFunctor #-}

module GI.Gtk.Declarative.Attributes.Custom
  ( CustomAttribute (..) ) where

--import GI.Gtk.Declarative.EventSource

data CustomAttribute widget internalState event =
    CustomAttribute
      { attrCreate :: widget -> IO internalState
      , attrPatch :: widget -> internalState -> IO internalState
      , attrDestroy :: widget -> internalState -> IO ()
      --, attrSubscribe :: widget -> internalState -> (event -> IO ()) -> IO Subscription
      }
    deriving (Functor)
