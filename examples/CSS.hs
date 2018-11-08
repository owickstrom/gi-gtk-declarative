{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module CSS where

import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )

import           Control.Concurrent.Async       ( async )
import           Control.Monad                  ( forM_
                                                , void
                                                )
import qualified GI.Gdk                        as Gdk
import           GI.Gtk                         ( Box(..)
                                                , Button(..)
                                                , Orientation(..)
                                                , Window(..)
                                                )
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

type State = Int

data Event
  = MoveTo Int
  | Closed

colors :: [Text]
colors = ["red", "green", "blue", "yellow"]

view' :: State -> AppView Event
view' si =
  bin Window [#title := "CSS Example", on #deleteEvent (const (True, Closed))]
    $ container Box [#orientation := OrientationVertical]
    $ boxChild True False 10
    $ container Box [#orientation := OrientationHorizontal]
    $ forM_ (zip [0 ..] colors)
    $ \(i, color) ->
        boxChild True False 10
          $ let cs = if i == si then ["selected", color] else [color]
            in  widget Button
                       [#label := color, on #clicked (MoveTo i), classes cs]

update' :: State -> Event -> Transition State Event
update' s (MoveTo i)
  | i >= 0 && i < length colors = Transition i (return Nothing)
  | otherwise                   = Transition s (return Nothing)
update' _ Closed = Exit

styles :: ByteString
styles = mconcat
  [ "button { border: 2px solid gray; font-weight: 800; }"
  , ".selected { background: white; border: 2px solid black; }"
  -- Specific color classes:
  , ".red { color: red; }"
  , ".green { color: green; }"
  , ".blue { color: blue; }"
  , ".yellow { color: goldenrod; }"
  ]

main :: IO ()
main = do
  void $ Gtk.init Nothing

  -- Set up screen and CSS provider
  screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
  p      <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData p styles
  Gtk.styleContextAddProviderForScreen
    screen
    p
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  -- Start main loop
  void . async $ do
    runLoop app
    Gtk.mainQuit
  Gtk.main
 where
  app = App {view = view', update = update', inputs = [], initialState = 0}
