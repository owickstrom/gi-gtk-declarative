-- | Helpers for modifying CSS classes in 'Gtk.StyleContext'
-- objects.

module GI.Gtk.Declarative.CSS where

import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Text    (Text)
import qualified GI.Gtk       as Gtk

-- | A set of CSS classes.
type ClassSet = HashSet Text

-- | Add the classes to the widget's style context.
addClasses
  :: Gtk.IsWidget w
  => w        -- ^ The widget to add classes to.
  -> ClassSet -- ^ New classes to add.
  -> IO ()
addClasses widget cs = do
  sc <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextAddClass sc) cs

-- | Replace all classes in 'old' with the ones in 'old' in the widget's style
-- context.
replaceClasses
  :: Gtk.IsWidget w
  => w        -- ^ The widget to replace classes in.
  -> ClassSet -- ^ Old classes to replace.
  -> ClassSet -- ^ New classes to replace with.
  -> IO ()
replaceClasses widget old new = do
  sc <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextRemoveClass sc) (HashSet.difference old new)
  mapM_ (Gtk.styleContextAddClass sc)    (HashSet.difference new old)
