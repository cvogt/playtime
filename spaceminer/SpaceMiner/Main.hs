module Main where

import qualified Data.Map as Map
import My.IO
import My.Prelude
import Playtime
import SpaceMiner.GameState
import SpaceMiner.Visualize

dim :: Dimensions
dim = Dimensions {width = 320, height = 240}

gameDir :: FilePath
gameDir = "spaceminer"

srcFiles :: [FilePath]
srcFiles =
  [ gameDir </> "SpaceMiner/Main.hs",
    gameDir </> "SpaceMiner/GameState.hs",
    gameDir </> "SpaceMiner/Visualize.hs"
  ]

--   foldl (.) id (flap [applyToEngineState, applyToGameState] event')
main :: IO ()
main =
  playtimeLiveCode makeEngineConfig "SpaceMiner.Main" "makeEngineConfig" (gameDir </> "SpaceMiner") srcFiles

makeEngineConfig :: LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  recoveredGameState <- startLiveCode liveCodeState
  gameStateMVar <- newMVar $ fromMaybe (makeInitialGameState dim) recoveredGameState

  pure $
    EngineConfig
      { ecDim = dim,
        ecScale = 3,
        ecVisualize = \tx es -> visualize tx es <$> readMVar gameStateMVar,
        ecStepGameState = \es event -> modifyMVar_ gameStateMVar $ \old_gs -> do
          let new_gs = stepGameStatePure old_gs es event
          liveCodeSwitch liveCodeState new_gs
          saveMay es new_gs
          fromMaybe new_gs <$> loadMay es,
        ecCheckIfContinue = pure . not . gameExitRequested,
        ecGameDebugInfo = \_ -> do
          GameState {..} <- readMVar gameStateMVar
          let Pos x' y' = gsMainCharacterPosition
          pure $
            [ "collisions: " <> show gsCollisions,
              "candidates: " <> show gsCandidates,
              "main char: " <> show (x', y'),
              "last places sprite location: " <> show gsLastPlacement,
              "sprite count floor: " <> show (Map.size $ unBoard gsFloor),
              "sprite count room: " <> show (Map.size $ unBoard gsRoom)
            ]
      }
