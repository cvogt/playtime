module Playtime.ConcurrentState where

import My.IO
import My.Prelude
import Playtime.Types

data ConcurrentState gameState = ConcurrentState
  { csSpritePlacementTime :: MVar [(SystemTime, SystemTime)],
    csGameState :: MVar (EngineState, gameState),
    csTimeStep :: MVar [(SystemTime, SystemTime)],
    csTimeGL :: MVar [(SystemTime, SystemTime)],
    csTimeRender :: MVar [(SystemTime, SystemTime)]
  }

makeInitialConcurrentState :: EngineState -> gameState -> IO (ConcurrentState gameState)
makeInitialConcurrentState es gs = do
  csSpritePlacementTime <- newMVar []
  csGameState <- newMVar (es, gs)
  csTimeStep <- newMVar []
  csTimeGL <- newMVar []
  csTimeRender <- newMVar []
  pure $ ConcurrentState {..}
