{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO
import System.Environment

import qualified AddBoxes
import qualified FileChooserButton
import qualified Hello
import qualified ListBox
import qualified Functor

main :: IO ()
main =
  let examples = [ ("AddBoxes", AddBoxes.main)
                 , ("FileChooserButton", FileChooserButton.main)
                 , ("Hello", Hello.main)
                 , ("ListBox", ListBox.main)
                 , ("Functor", Functor.main)
                 ]
  in getArgs >>= \case
    [example] ->
      case lookup example examples of
        Just main' -> main'
        Nothing -> hPutStrLn stderr ("No example available with name: " <> example)
    _ -> hPutStrLn stderr "Usage: gi-gtk-declarative-example NAME"
