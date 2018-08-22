{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module Hello where

import           Control.Concurrent
import           Control.Monad
import           Data.Text          (Text)
import           GHC.Exts

import           GI.Gtk.Declarative hiding (main)
import qualified GI.Gtk.Declarative as Gtk
import           GI.Gtk.Declarative.App.Simple

data Model = Initial | Running Text Bool

data Event = Greet Text

helloView :: Model -> Markup Event
helloView Initial = node Label [#label := "Nothing here yet."]
helloView (Running who flipped) = container Box [] $
  op
  [ BoxChild True True 0 (node Label [#label := "This is a sample application."])
  , BoxChild True True 0 (node Label [#label := who])
  ]
  where
    op :: [BoxChild Event] -> Children BoxChild Event
    op = fromList . if flipped then reverse else id

update' :: Model -> Event -> (Model, IO (Maybe Event))
update' Initial (Greet who) = (Running who False, return Nothing)
update' (Running _ flipped) (Greet who) = (Running who (not flipped), return Nothing)

main :: IO ()
main = do
  -- Basic setup.
  void $ Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  void (Gtk.onWidgetDestroy window mainQuit)

  -- Customize the top window.
  Gtk.windowSetTitle window "Sample gi-gtk-declarative app!"
  Gtk.windowResize window 640 480

  input' <- newChan

  void . forkIO $
    forM_ (cycle ["Joe", "Mike"]) $ \n ->
      threadDelay 1000000 *> writeChan input' (Greet ("Hello, " <> n))

  let app = App { view = helloView
                , update = update'
                , input = input'
                }

  void . forkIO $ runInWindow window app Initial

  -- Let's do it!
  Gtk.main
