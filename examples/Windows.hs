{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Windows where

import           Control.Concurrent            (threadDelay)
import           Control.Monad                 (void)
import           Data.Text                     (pack)
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as Vector
import           Pipes.Prelude                 (repeatM)

import           GI.Gtk                        (Box (..), Button (..),
                                                Label (..), Orientation (..),
                                                Window (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

type State = Vector (Maybe Int)

data Event
  = IncrAll
  | AddWindow
  | CloseWindow Int
  | RemoveWindow
  | Closed

view' :: State -> AppView Window Event
view' ns =
  bin
      Window
      [ #title := "Windows"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 200
      , #heightRequest := 300
      ]
    $  container Box [#orientation := OrientationVertical, #spacing := 4, #margin := 4]
    $  [addButton, removeButton ns]
    <> Vector.imap windowLabel ns

addButton :: BoxChild Event
addButton = BoxChild defaultBoxChildProperties
  $ widget Button [#label := "Add Window", on #clicked AddWindow]

removeButton :: State -> BoxChild Event
removeButton ns = BoxChild defaultBoxChildProperties $ widget
  Button
  [ #label := "Remove Window"
  , #sensitive := (ns /= mempty)
  , on #clicked RemoveWindow
  ]

windowLabel :: Int -> Maybe Int -> BoxChild Event
windowLabel i n =
  BoxChild defaultBoxChildProperties { padding = 4 }
    $ windowHost (window i <$> n)
    $ windowChild i n

window :: Int -> Int -> Bin Window Event
window i x = bin
  Window
  [ #title := pack ("Window " <> show i)
  , on #deleteEvent (const (True, CloseWindow i))
  ]
  (widget Label [#label := pack ("Open for " <> show x <> " seconds")])

windowChild :: Int -> Maybe Int -> Widget Event
windowChild i = \case
  Nothing -> widget Label [#label := pack ("Window " <> show i <> " Closed")]
  Just _  -> widget Label [#label := pack ("Window " <> show i <> " Open")]

update' :: State -> Event -> Transition State Event
update' ns = \case
  IncrAll       -> Transition (fmap succ <$> ns) (return Nothing)
  AddWindow     -> Transition (ns `Vector.snoc` Just 1) (return Nothing)
  CloseWindow i -> Transition
    (Vector.imap (\i' n -> if i' == i then Nothing else n) ns)
    (return Nothing)
  RemoveWindow -> Transition (Vector.init ns) (return Nothing)
  Closed       -> Exit

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = [incrPeriodically]
  , initialState = []
  }
  where incrPeriodically = repeatM $ IncrAll <$ threadDelay (1000 * 1000)
