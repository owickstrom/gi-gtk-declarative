{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Exit where

import           Control.Concurrent            (threadDelay)
import           Data.Functor                  (($>))
import qualified Data.Text                     as Text

import           GI.Gtk                        (Button (..), Label (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data State = Running | ExitingIn Int

data Event = ExitApplication | CountDownExit

view' :: State -> Widget Event
view' Running =
  widget Button [#label := "Exit", on #clicked ExitApplication]
view' (ExitingIn sec) =
  widget Label [#label := ("Exiting in " <> Text.pack (show sec) <> " seconds.")]

countDown :: IO (Maybe Event)
countDown = threadDelay oneSec $> Just CountDownExit
  where
    oneSec :: Int
    oneSec = 1000000

update' :: State -> Event -> Continuation State Event
update' Running ExitApplication = Continue (ExitingIn 3) countDown
update' Running _               = Continue Running (pure Nothing)
update' (ExitingIn 1) _         = Exit
update' (ExitingIn sec) _       = Continue (ExitingIn (pred sec)) countDown

main :: IO ()
main =
  run
    "Hello"
    (Just (640, 480))
    App {view = view', update = update', inputs = [], initialState = Running}
