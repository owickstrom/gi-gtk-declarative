{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Criterion.Main
import           Data.Text
import qualified GI.Gdk                        as Gdk
import qualified GI.GLib.Constants             as GLib

import           GI.Gtk                         ( Box(..)
                                                , Window(..)
                                                , Label(..)
                                                )
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.State

testView :: [Int] -> Widget ()
testView ns = bin Window [] $ container Box [] $ forM_ ns $ \n ->
  boxChild True True 10
    $ widget Label [#label := pack (show n), classes ["a", "b"]]

testPatch state oldView newView = case patch state oldView newView of
  Modify ma -> do
    ret <- newEmptyMVar
    void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
      ma >>= putMVar ret
      return False
    takeMVar ret
  _ -> return ()

main :: IO ()
main = do
  _      <- Gtk.init Nothing
  let initialView = testView [1 .. 100]
  initialState <- create initialView
  #showAll (stateTreeNodeWidget initialState)
  _ <- forkOS $ do
    defaultMain
      [ bgroup
          "patch"
          [ bench "Modify (equal)" . whnfIO . replicateM_ 10 $
            void $ testPatch initialState initialView initialView
            void $ testPatch initialState initialView initialView
          , bench "Modify (diff)" . whnfIO . replicateM_ 10 $
            void $ testPatch initialState initialView initialView
            void $ testPatch initialState initialView (testView [2 .. 101])
          ]
      ]
    Gtk.mainQuit
  Gtk.main
