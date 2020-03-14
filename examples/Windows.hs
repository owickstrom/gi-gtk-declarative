{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Windows where

import           Control.Concurrent                          (threadDelay)
import           Control.Monad                               (void)
import           Data.Functor                                ((<&>))
import           Data.Text                                   (pack)
import           Data.UUID                                   (UUID)
import           Data.Vector                                 (Vector)
import qualified Data.Vector                                 as Vector
import           Pipes.Prelude                               (repeatM)
import           System.Random                               (randomIO)

import           GI.Gtk                                      (Box (..),
                                                              Button (..),
                                                              Label (..),
                                                              Orientation (..),
                                                              Window (..),
                                                              WindowPosition (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.Attributes.Custom.Window (presentWindow,
                                                              window)

data WindowState = WindowState
  { windowStateKey       :: UUID
  , windowStateCount     :: Int
  , windowStatePresented :: Int
  }

type State = Vector WindowState

data Event
  = IncrAll
  | AddWindow UUID
  | PresentWindow UUID
  | RemoveWindow UUID
  | Closed

view' :: State -> AppView Window Event
view' ws =
  bin Window
    ([ #title := "Windows"
    , on #deleteEvent (const (True, Closed))
    , #widthRequest := 400
    , #heightRequest := 300
    , #windowPosition := WindowPositionCenter
    ] <> (windowAttr <$> ws)) $
    container
      Box
      [#orientation := OrientationVertical, #spacing := 4, #margin := 4] $
      addButton `Vector.cons` (windowButton <$> ws)

addButton :: BoxChild Event
addButton = BoxChild defaultBoxChildProperties
  $ widget Button
      [ #label := "Add Window"
      , onM #clicked (const (AddWindow <$> randomIO))
      ]

windowButton :: WindowState -> BoxChild Event
windowButton WindowState {..} =
  BoxChild defaultBoxChildProperties $
    widget
    Button
    [ #label := pack ("Present window " <> show windowStateKey)
    , on #clicked $ PresentWindow windowStateKey
    ]

windowAttr :: WindowState -> Attribute widget Event
windowAttr WindowState {..} = window windowStateKey $ bin
  Window
  [ #title := pack (show windowStateKey)
  , on #deleteEvent (const (True, RemoveWindow windowStateKey))
  , #widthRequest := 400
  , #heightRequest := 250
  , #windowPosition := WindowPositionCenter
  , presentWindow windowStatePresented
  ] $
  widget Label
    [#label := pack ("Open for " <> show windowStateCount <> " seconds")]

update' :: State -> Event -> Transition State Event
update' ws = \case
  IncrAll -> Transition
    (ws <&> \w -> w { windowStateCount = windowStateCount w + 1})
    (pure Nothing)
  AddWindow key -> Transition
    (ws `Vector.snoc` WindowState key 1 1)
    (pure Nothing)
  PresentWindow key -> Transition
    (ws <&> \w ->
      if windowStateKey w == key
      then w { windowStatePresented = windowStatePresented w + 1 }
      else w)
    (pure Nothing)
  RemoveWindow key -> Transition
    (Vector.filter (\w -> windowStateKey w /= key) ws)
    (pure Nothing)
  Closed->
    Exit

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = [incrPeriodically]
  , initialState = []
  }
  where incrPeriodically = repeatM $ IncrAll <$ threadDelay (1000 * 1000)
