module Platformer.Main where

import qualified Data.Map as Map
import Data.Time.Clock.System
import My.IO
import My.Prelude
import Platformer.GameState
import Platformer.Graphics
import Playtime

--   foldl (.) id (flap [applyToEngineState, applyToGameState] event')

dim :: Dimensions
dim = Dimensions {width = 320, height = 240}

main :: IO ()
main = do
  playtime =<< newMVar =<< makeEngineConfig <$> newMVar (makeInitialGameState dim)

makeEngineConfig :: MVar GameState -> EngineConfig
makeEngineConfig gameStateMVar =
  EngineConfig
    { ecDim = dim,
      ecScale = 3,
      ecComputeSpritePlacements' = \tx es -> computeSpritePlacements tx es <$> readMVar gameStateMVar,
      ecStepGameState = \es event -> modifyMVar_ gameStateMVar $ \gs -> pure $ stepGameState' gs es event,
      ecCheckIfContinue = pure . not . gameExitRequested,
      ecGameDebugInfo = \_ -> do
        GameState {..} <- readMVar gameStateMVar
        let Pos x' y' = gsMainCharacterPosition
        pure $
          [ "gsVelocityY: " <> show gsVelocityY,
            "collisions: " <> show gsCollisions,
            "main char: " <> show (x', y'),
            "sprite count room: " <> show (Map.size $ unBoard gsRoom)
          ]
    }

tests :: IO ()
tests = do
  let igs =
        (makeInitialGameState dim)
          { gsVelocityY = 0.33,
            gsMainCharacterPosition = Pos 0 (-7),
            gsRoom = Board $ mapFromList $ (,FloorPlate) <$> [Pos (-6) 5, Pos 6 5]
          }
  time <- getSystemTime
  let egs = makeInitialEngineState 3 dim time
  let igs' = stepGameState' igs egs $ RenderEvent (time {systemNanoseconds = systemNanoseconds time + 1000000000})
  when (gsMainCharacterPosition igs' /= gsMainCharacterPosition igs) $ do
    putStrLn $ "FAIL: " <> show igs'
