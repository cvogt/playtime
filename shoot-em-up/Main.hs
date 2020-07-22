module Main where

import My.IO
import qualified ShootEmUp.Main

main :: IO ()
main = do
  putStrLn "running tests"
  ShootEmUp.Main.tests
  putStrLn "starting main"
  ShootEmUp.Main.main
