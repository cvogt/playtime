module SpaceMiner.ConcurrentState where

import My.IO
import My.Prelude
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
