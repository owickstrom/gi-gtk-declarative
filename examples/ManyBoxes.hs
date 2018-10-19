{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module ManyBoxes where

import           Data.Text                     (pack)

import           GI.Gtk                        (Box(..), Button(..), ScrolledWindow(..))
import           GI.Gtk.Declarative
import Control.Monad (forM_)
import           GI.Gtk.Declarative.App.Simple

type State = [Int]

data Event = IncrAll

view' :: State -> Widget Event
view' ns =
  bin ScrolledWindow [] $
    container Box [] $ forM_ ns $ \n ->
      boxChild False False 10 $
      widget Button [#label := pack (show n), on #clicked IncrAll]

update' :: State -> Event -> Transition State Event
update' ns IncrAll = Transition (map succ ns) (return Nothing)

main :: IO ()
main = run
  "Many Numbers"
  (Just (640, 480))
  App
    { view         = view'
    , update       = update'
    , inputs       = []
    , initialState = ([0..500] :: [Int])
    }
