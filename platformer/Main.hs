module Main where

import My.IO
import qualified Platformer.Main

main :: IO ()
main = do
  putStrLn "running tests"
  Platformer.Main.tests
  putStrLn "starting main"
  Platformer.Main.main
