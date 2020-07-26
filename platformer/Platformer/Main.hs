module Platformer.Main where

import qualified Data.Map as Map
import Data.Time.Clock.System
import My.IO
import My.Prelude
import Platformer.GameState
import Platformer.Visualize
import Playtime

--   foldl (.) id (flap [applyToEngineState, applyToGameState] event')

gameDir :: FilePath
gameDir = "platformer"

srcFiles :: [FilePath]
srcFiles =
  [ gameDir </> "Platformer/Main.hs",
    gameDir </> "Platformer/GameState.hs",
    gameDir </> "Platformer/Graphics.hs"
  ]

--   foldl (.) id (flap [applyToEngineState, applyToGameState] event')
main :: IO ()
main =
  playtimeLiveCode makeEngineConfig "Platformer.Main" "makeEngineConfig" (gameDir </> "Platformer") srcFiles

dim :: Dimensions
dim = Dimensions {width = 320, height = 240}

makeEngineConfig :: LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  recoveredGameState <- startLiveCode liveCodeState
  gameStateMVar <- newMVar =<< maybe (pure $ makeInitialGameState dim) pure recoveredGameState

  pure $
    EngineConfig
      { ecDim = dim,
        ecScale = 3,
        ecVisualize = \tx es -> visualize tx es <$> readMVar gameStateMVar,
        ecStepGameState = \es event -> modifyMVar_ gameStateMVar $ \gs -> do
          let new_gs = stepGameState' gs es event
          liveCodeSwitch liveCodeState new_gs
          pure new_gs,
        ecCheckIfContinue = pure . not . gameExitRequested,
        ecGameDebugInfo = \_ -> do
          GameState {..} <- readMVar gameStateMVar
          let Pos x' y' = gsMainCharacterPosition
          pure $
            [ "collisions: " <> show gsCollisions,
              "main char: " <> show (x', y'),
              "gsVelocityX: " <> show gsVelocityX,
              "gsVelocityY: " <> show gsVelocityY,
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
