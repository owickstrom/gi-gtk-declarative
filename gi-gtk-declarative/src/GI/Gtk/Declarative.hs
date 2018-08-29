-- | The declarative layer on top of GTK+ lets you describe your user
-- interface as a declarative hierarchy of objects, using data
-- structures and pure functions. The patching algorithm performs
-- minimal updates to GTK+ widgets using the underlying imperative
-- operations.
module GI.Gtk.Declarative
  ( module Export
  )
where

import           GI.Gtk.Declarative.Attributes    as Export
import           GI.Gtk.Declarative.Bin           as Export
import           GI.Gtk.Declarative.Container     as Export
import           GI.Gtk.Declarative.Container.Box as Export
import           GI.Gtk.Declarative.CSS           as Export
import           GI.Gtk.Declarative.Markup        as Export
import           GI.Gtk.Declarative.Patch         as Export
import           GI.Gtk.Declarative.SingleWidget  as Export
