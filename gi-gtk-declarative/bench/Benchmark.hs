{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Criterion.Main
import           Data.Text
import qualified GI.Gdk             as Gdk
import qualified GI.GLib.Constants  as GLib

import           GI.Gtk             (Box (..), Label (..))
import qualified GI.Gtk             as Gtk
import           GI.Gtk.Declarative

testView :: [Int] -> Widget ()
testView ns = container Box [] $ forM_ ns $ \n ->
  boxChild True True 10 $ widget Label [#label := pack (show n), classes ["a", "b"]]

testPatch widget oldView newView = case patch oldView newView of
  Modify f -> do
    ret <- newEmptyMVar
    void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
      f widget
      putMVar ret ()
      return False
    takeMVar ret
  _ -> return ()

main :: IO ()
main = do
  _      <- Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  let initialView = testView [1 .. 100]
  widget <- create initialView
  #add window widget
  #showAll window
  _ <- forkOS $ do
    defaultMain
      [ bgroup
          "patch"
          [ bench "Modify (equal)" . whnfIO . replicateM_ 10 $
            testPatch widget initialView initialView
          , bench "Modify (diff)" . whnfIO . replicateM_ 10 $
            testPatch widget initialView (testView [2 .. 101])
          ]
      ]
    Gtk.mainQuit
  Gtk.main
