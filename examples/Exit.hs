{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Exit where

import           Control.Concurrent            (threadDelay)
import           Control.Monad                 (void)
import           Data.Functor                  (($>))
import qualified Data.Text                     as Text

import           GI.Gtk                        (Button (..), Label (..),
                                                Window (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data State = Running | ExitingIn Int

data Event = ExitApplication | CountDownExit

view' :: State -> AppView Window Event
view' s =
  bin Window [#title := "Exit", on #deleteEvent (const (True, ExitApplication)), #widthRequest := 400, #heightRequest := 300]
    $ case s of
        Running ->
          widget Button [#label := "Exit", on #clicked ExitApplication]
        ExitingIn sec -> widget
          Label
          [#label := ("Exiting in " <> Text.pack (show sec) <> " seconds.")]

countDown :: IO (Maybe Event)
countDown = threadDelay oneSec $> Just CountDownExit
 where
  oneSec :: Int
  oneSec = 1000000

update' :: State -> Event -> Transition State Event
update' Running       ExitApplication = Transition (ExitingIn 3) countDown
update' Running       _               = Transition Running (pure Nothing)
update' (ExitingIn 1) CountDownExit   = Exit
update' (ExitingIn sec) CountDownExit =
  Transition (ExitingIn (pred sec)) countDown
update' s@ExitingIn{} ExitApplication   = Transition s (pure Nothing)

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = Running
  }
