module Playtime.ConcurrentState where

import My.IO
import My.Prelude
import Playtime.EngineState

data ConcurrentState = ConcurrentState
  { csEngineState :: MVar EngineState,
    csTimeStep :: MVar [Double],
    csTimeGL :: MVar [Double],
    csTimeRender :: MVar [Double],
    csTimeVisualize :: MVar [Double]
  }

makeInitialConcurrentState :: EngineState -> IO ConcurrentState
makeInitialConcurrentState es = do
  csEngineState <- newMVar es
  csTimeStep <- newMVar []
  csTimeGL <- newMVar []
  csTimeRender <- newMVar []
  csTimeVisualize <- newMVar []
  pure $ ConcurrentState {..}
