module Playtime.ConcurrentState where

import My.IO
import My.Prelude
import Playtime.Types

data ConcurrentState = ConcurrentState
  { csSpritePlacementTime :: MVar [(SystemTime, SystemTime)],
    csEngineState :: MVar EngineState,
    csTimeStep :: MVar [(SystemTime, SystemTime)],
    csTimeGL :: MVar [(SystemTime, SystemTime)],
    csTimeRender :: MVar [(SystemTime, SystemTime)]
  }

makeInitialConcurrentState :: EngineState -> IO ConcurrentState
makeInitialConcurrentState es = do
  csSpritePlacementTime <- newMVar []
  csEngineState <- newMVar es
  csTimeStep <- newMVar []
  csTimeGL <- newMVar []
  csTimeRender <- newMVar []
  pure $ ConcurrentState {..}
