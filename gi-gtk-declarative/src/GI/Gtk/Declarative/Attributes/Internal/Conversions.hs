{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GI.Gtk.Declarative.Attributes.Internal.Conversions
  ( ToGtkCallback(..)
  )
where

import           Control.Monad                            ( void )
import           Data.Functor                             ( ($>) )
import           Data.Functor.Identity

import           GI.Gtk.Declarative.Attributes.Internal.Callback

-- * GTK+ Callback Conversions

-- | Internal class for converting 'Callback's to gi-gtk callbacks.
class ToGtkCallback gtkCallback purity where
  -- | Converts a 'Callback', i.e. the internal encoding of a pure or an impure
  -- callback, back to a GTK+ callback. Impure callbacks will also receive a
  -- 'widget' as the last argument.
  toGtkCallback
    :: Callback gtkCallback widget purity event
    -> widget
    -> (event -> IO ())
    -> gtkCallback

instance ToGtkCallback (IO ()) Pure where
  toGtkCallback (PureCallback (OnlyEvent e)) _ f = void (f (runIdentity e))

instance ToGtkCallback (IO Bool) Pure where
  toGtkCallback (PureCallback (ReturnAndEvent re)) _ f =
    let (r, e) = runIdentity re
    in f e $> r

instance ToGtkCallback (IO ()) Impure where
  toGtkCallback (ImpureCallback r) w f =
    let OnlyEvent me = r w
    in me >>= f

instance ToGtkCallback (IO Bool) Impure where
  toGtkCallback (ImpureCallback r) w f = do
    let ReturnAndEvent re = r w
    (r', e) <- re
    f e
    return r'

instance ToGtkCallback y purity => ToGtkCallback (x -> y) purity where
  toGtkCallback (CallbackFunction cb) f w x = toGtkCallback (cb x) f w
