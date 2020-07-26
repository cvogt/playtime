module Platformer.Main where

import qualified Data.Map as Map
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
  gameStateMVar <- newMVar $ fromMaybe (makeInitialGameState dim) recoveredGameState

  pure $
    EngineConfig
      { ecDim = dim,
        ecScale = 3,
        ecVisualize = \tx es -> visualize tx es <$> readMVar gameStateMVar,
        ecStepGameState = \es event -> modifyMVar_ gameStateMVar $ \gs -> do
          let new_gs = stepGameStatePure gs es event
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
