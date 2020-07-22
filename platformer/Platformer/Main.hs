module Platformer.Main where

import qualified Data.Map as Map
import Data.Time.Clock.System
import Game
import My.IO
import My.Prelude
import Platformer.GameState
import Platformer.Graphics
import Playtime
import Playtime.Textures
import Playtime.Types
import System.Random

--   foldl (.) id (flap [applyToEngineState, applyToGameState] event')
main :: IO ()
main =
  let igs = makeInitialGameState dim
      dim = Dimensions {width = 320, height = 240} -- logical pixel resolution
   in playtime $ EngineConfig igs dim 3 stepGameState' computeSpritePlacements $ \EngineState {..} GameState {..} ->
        let Pos x' y' = gsMainCharacterPosition
         in [ "gsVelocityY: " <> show gsVelocityY,
              "collisions: " <> show gsCollisions,
              "main char: " <> show (x', y'),
              "sprite count room: " <> show (Map.size $ unBoard gsRoom)
            ]

tests :: IO ()
tests = do
  let dim = Dimensions {width = 320, height = 240} -- logical pixel resolution
  let igs =
        (makeInitialGameState dim)
          { gsVelocityY = 0.33,
            gsMainCharacterPosition = Pos 0 (-7),
            gsRoom = Board $ mapFromList $ (,FloorPlate) <$> [Pos (-6) 5, Pos 6 5]
          }
  time <- getSystemTime
  let egs = makeInitialEngineState 3 dim time
  let igs' = stepGameState' (mkStdGen 17) egs igs $ RenderEvent (time {systemNanoseconds = systemNanoseconds time + 1000000000})
  when (gsMainCharacterPosition igs' /= gsMainCharacterPosition igs) $ do
    putStrLn $ "FAIL: " <> show igs'
