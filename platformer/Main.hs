module Main where

import My.IO
import qualified Platformer.Main

main :: IO ()
main = do
  Platformer.Main.tests
  Platformer.Main.main
