{-# LANGUAGE LambdaCase #-}
module Main where

import           System.Environment
import           System.IO

import           Data.List (intercalate)

import qualified AddBoxes
import qualified Exit
import qualified FileChooserButton
import qualified Functor
import qualified Hello
import qualified ListBox
import qualified Window
import qualified MenuBar

main :: IO ()
main =
  let examples = [ ("AddBoxes", AddBoxes.main)
                 , ("FileChooserButton", FileChooserButton.main)
                 , ("Hello", Hello.main)
                 , ("ListBox", ListBox.main)
                 , ("Functor", Functor.main)
                 , ("Exit", Exit.main)
                 , ("Window", Window.main)
                 , ("MenuBar", MenuBar.main)
                 ]
      exampleNames = intercalate ", " $ fst <$> examples
  in getArgs >>= \case
    [example] ->
      case lookup example examples of
        Just main' -> main'
        Nothing -> hPutStrLn stderr ("No example available with name: " <> example)
    _ -> hPutStrLn stderr $ "Usage: gi-gtk-declarative-example EXAMPLE\nExamples: " <> exampleNames
