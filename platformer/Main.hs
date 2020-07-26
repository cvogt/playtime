module Main where

import Data.Time.Clock.System
import My.IO
import My.Prelude
import Platformer.GameState
import qualified Platformer.Main
import Playtime

main :: IO ()
main = do
  putStrLn "running tests"
  tests
  putStrLn "starting main"
  Platformer.Main.main

tests :: IO ()
tests = do
  let dim = Dimensions {width = 320, height = 240}
      igs =
        (makeInitialGameState dim)
          { gsVelocityY = 0.33,
            gsMainCharacterPosition = Pos 0 (-7),
            gsRoom = Board $ mapFromList $ (,FloorPlate) <$> [Pos (-6) 5, Pos 6 5]
          }
  time <- getSystemTime
  let egs = makeInitialEngineState 3 dim time
  let igs' = stepGameStatePure igs egs $ RenderEvent (time {systemNanoseconds = systemNanoseconds time + 1000000000})
  when (gsMainCharacterPosition igs' /= gsMainCharacterPosition igs) $ do
    putStrLn $ "FAIL: " <> show igs'
