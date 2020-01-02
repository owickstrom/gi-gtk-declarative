{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Criterion.Main
import           Data.Functor                   ( (<&>) )
import           Data.Text
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector
import qualified GI.Gdk                        as Gdk
import qualified GI.GLib.Constants             as GLib

import           GI.Gtk                         ( Box(..)
                                                , Label(..)
                                                , Window(..)
                                                )
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.State

testView :: Vector Int -> Widget ()
testView ns = bin Window [] $ container Box [] $ ns <&> \n ->
  BoxChild defaultBoxChildProperties { expand  = True
                                     , fill    = True
                                     , padding = 10
                                     }
    $ widget Label [#label := pack (show n), classes ["a", "b"]]

testPatch
  :: Patchable widget => SomeState -> widget e1 -> widget e2 -> IO SomeState
testPatch state oldView newView = case patch state oldView newView of
  Modify ma -> do
    ret <- newEmptyMVar
    void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
      ma >>= putMVar ret
      return False
    takeMVar ret
  _ -> error "Expected a modification."

main :: IO ()
main = do
  _ <- Gtk.init Nothing
  let initialView = testView (Vector.enumFromN 1 100)
  initialState <- create initialView
  #showAll =<< someStateWidget initialState
  _ <- forkOS $ do
    defaultMain
      [ bgroup
          "patch"
          [ bench "Modify (equal)" . whnfIO . replicateM_ 10 $ do
            s1 <- testPatch initialState initialView initialView
            void $ testPatch s1 initialView initialView
          , bench "Modify (diff)" . whnfIO . replicateM_ 10 $ do
            s1 <- testPatch initialState initialView initialView
            void $ testPatch s1 initialView (testView (Vector.enumFromN 2 101))
          ]
      ]
    Gtk.mainQuit
  Gtk.main
