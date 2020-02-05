-- | The declarative layer on top of GTK+ lets you describe your user
-- interface as a declarative hierarchy of objects, using data
-- structures and pure functions. You can leverage the declarative event
-- handling to build reusable widgets. The "Patch" typeclass, and the
-- instances provided by this library, performs minimal updates to GTK+ widgets
-- using the underlying imperative operations, so that your rendering can always
-- be a pure function your state to a "Widget".
module GI.Gtk.Declarative
  ( module Export
  )
where

import           GI.Gtk.Declarative.Attributes as Export
import           GI.Gtk.Declarative.Bin        as Export
                                                ( Bin
                                                , bin
                                                )
import           GI.Gtk.Declarative.Container  as Export
                                                ( Container
                                                , container
                                                )
import           GI.Gtk.Declarative.Container.Box
                                               as Export
import           GI.Gtk.Declarative.Container.Grid
                                               as Export
                                                ( )
import           GI.Gtk.Declarative.Container.ListBox
                                               as Export
                                                ( )
import           GI.Gtk.Declarative.Container.MenuItem
                                               as Export
import           GI.Gtk.Declarative.Container.Paned
                                               as Export
import           GI.Gtk.Declarative.Container.Notebook
                                               as Export
import           GI.Gtk.Declarative.CustomWidget
                                               as Export
import           GI.Gtk.Declarative.Patch      as Export
import           GI.Gtk.Declarative.SingleWidget
                                               as Export
import           GI.Gtk.Declarative.Widget     as Export
import           GI.Gtk.Declarative.Widget.Conversions
                                               as Export
                                                ( )
