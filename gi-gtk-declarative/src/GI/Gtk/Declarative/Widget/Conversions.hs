{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Orphan instances for automatically converting between different
-- widget types. As these use 'UndecidableInstances', they are kept
-- here in solitary confinement.
module GI.Gtk.Declarative.Widget.Conversions where

import           GI.Gtk.Declarative.Container.Box
import           GI.Gtk.Declarative.Widget

-- | Any widget that can be converted to a 'Widget' can be wrapped
-- as a 'BoxChild' with the default properties.
instance FromWidget widget Widget => FromWidget widget BoxChild where
  fromWidget = BoxChild defaultBoxChildProperties . fromWidget
