{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module AddBoxes where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text                     as Text
import GHC.Exts

import           GI.Gtk.Declarative                hiding ( main )
import qualified GI.Gtk.Declarative            as Gtk

import           MainLoop

type AddBoxCommand = Either () ()

type Model = ([Int], [Int])

addBoxesView :: Chan AddBoxCommand -> Model -> Markup ()
addBoxesView events (lefts, rights) = container
  ScrolledWindow
  [ #hscrollbarPolicy := PolicyTypeAutomatic
  , #vscrollbarPolicy := PolicyTypeNever
  ]
  (container
    Box
    [#orientation := OrientationVertical]
    ([ renderLane (const (writeChan events (Left ())))  lefts
    , renderLane (const (writeChan events (Right ()))) rights
    ] :: Children BoxChild ())
  )
 where
  renderLane :: (Button -> ButtonClickedCallback) -> [Int] -> BoxChild ()
  renderLane onClick children = BoxChild
    True
    True
    10
    (container
      Box
      []
      (fromList
      ( BoxChild False
                 False
                 10
                 (node Button [#label := "Add", on #clicked onClick])
      : map renderChild children
      ))
    )
  renderChild :: Int -> BoxChild ()
  renderChild n =
    BoxChild True True 0 (node Label [#label := Text.pack ("Box " <> show n)])

commandLoop :: Chan AddBoxCommand -> Chan Model -> IO ()
commandLoop events models = do
  let initialModel = ([1], [2])
  writeChan models initialModel
  go 3 initialModel
 where
  go n (lefts, rights) = do
    event <- readChan events
    let newModel = case event of
          Left  () -> (lefts ++ [n], rights)
          Right () -> (lefts, rights ++ [n])
    writeChan models newModel
    go (succ n) newModel

main :: IO ()
main = do
  void $ Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  void (Gtk.onWidgetDestroy window mainQuit)

  Gtk.windowSetTitle window "AddBoxes"
  Gtk.windowResize window 640 480

  models <- newChan
  events <- newChan

  void (forkIO (commandLoop events models))

  -- And a thread for the main loop that listens for models, diffs the
  -- GUI, and re-renders the underlying GTK+ widgets when needed.
  void . forkIO $ mainLoop window models (addBoxesView events)

  -- Let's do it!
  Gtk.main
