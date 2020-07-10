module SpaceMiner.ConcurrentState where

import Graphics
import My.IO
import My.Prelude
import SpaceMiner.Debug
import SpaceMiner.Types

data ConcurrentState = ConcurrentState
  { csSpritePlacementTime :: MVar [(SystemTime, SystemTime)],
    csGameState :: MVar GameState,
    csGameLoopTime :: MVar [(SystemTime, SystemTime)],
    csRenderLoopTime :: MVar [(SystemTime, SystemTime)],
    csTotalLoopTime :: MVar [(SystemTime, SystemTime)]
  }

makeInitialConcurrentState :: GameState -> IO ConcurrentState
makeInitialConcurrentState gameState = do
  csSpritePlacementTime <- newMVar []
  csGameState <- newMVar gameState
  csGameLoopTime <- newMVar []
  csRenderLoopTime <- newMVar []
  csTotalLoopTime <- newMVar []
  pure $ ConcurrentState {..}

receiveSpritePlacements :: ConcurrentState -> GameState -> IO [TexturePlacements]
receiveSpritePlacements ConcurrentState {csSpritePlacementTime} gs = do
  let texturePlacements = computeSpritePlacements gs
  void $ trackTime csSpritePlacementTime $ pure texturePlacements
  pure texturePlacements
