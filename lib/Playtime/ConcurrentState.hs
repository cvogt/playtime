module Playtime.ConcurrentState where

import My.Extra
import My.IO
import My.Prelude
import Playtime.Types
import System.Random (newStdGen)

data ConcurrentState gameState = ConcurrentState
  { csSpritePlacementTime :: MVar [(SystemTime, SystemTime)],
    csGameState :: MVar (EngineState, gameState),
    csEngineStateTime :: MVar [(SystemTime, SystemTime)],
    csRng :: MVar StdGen,
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
  csRng <- newMVar =<< newStdGen -- FIXME: I think this generator is not thread safe
  pure $ ConcurrentState {..}
