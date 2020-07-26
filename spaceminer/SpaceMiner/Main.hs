module Main where

import qualified Data.Map as Map
import My.IO
import My.Prelude
import Playtime
import Playtime.Types
import SpaceMiner.GameState
import SpaceMiner.Graphics

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
          [ "collisions: " <> show gsCollisions,
            "candidates: " <> show gsCandidates,
            "main char: " <> show (x', y'),
            "last places sprite location: " <> show gsLastPlacement,
            "sprite count floor: " <> show (Map.size $ unBoard gsFloor),
            "sprite count room: " <> show (Map.size $ unBoard gsRoom)
          ]
    }
