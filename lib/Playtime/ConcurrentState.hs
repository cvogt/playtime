module Playtime.ConcurrentState where

import My.IO
import My.Prelude
import Playtime.Types

data ConcurrentState gameState = ConcurrentState
  { csSpritePlacementTime :: MVar [(SystemTime, SystemTime)],
    csGameState :: MVar (EngineState, gameState),
    csEngineStateTime :: MVar [(SystemTime, SystemTime)],
    csGameStateTime :: MVar [(SystemTime, SystemTime)],
    csRenderLoopTime :: MVar [(SystemTime, SystemTime)],
    csTotalLoopTime :: MVar [(SystemTime, SystemTime)]
  }

makeInitialConcurrentState :: (EngineState, gameState) -> IO (ConcurrentState gameState)
makeInitialConcurrentState gameState = do
  csSpritePlacementTime <- newMVar []
  csGameState <- newMVar gameState
  csEngineStateTime <- newMVar []
  csGameStateTime <- newMVar []
  csRenderLoopTime <- newMVar []
  csTotalLoopTime <- newMVar []
  pure $ ConcurrentState {..}
