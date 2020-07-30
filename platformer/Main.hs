module Main where

import Data.Time.Clock.System
import GHC.Err (error)
import My.IO
import My.Prelude
import Platformer.GameState
import qualified Platformer.Main
import Playtime

dim :: Dim
dim = (320, 240)

main :: IO ()
main = do
  putStrLn "running tests"
  -- tests
  putStrLn "starting main"
  Platformer.Main.main

tests :: IO ()
tests = do
  let igs =
        (makeInitialGameState dim)
          { gsVelocityY = 0.33,
            gsMainCharacter = (0, (-7)),
            gsRoom = Board $ mapFromList $ (,FloorPlate) <$> [(-6, 5), (6, 5)]
          }
  time <- getSystemTime
  let egs = makeInitialEngineState 3 dim time
  let igs' = stepGameStatePure (error "load textures in tests") igs egs $ RenderEvent (time {systemNanoseconds = systemNanoseconds time + 1000000000})
  when (gsMainCharacter igs' /= gsMainCharacter igs) $ do
    putStrLn $ "FAIL: " <> show igs'
