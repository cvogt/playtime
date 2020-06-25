module SpaceMiner.ConcurrentState where

import Control.Concurrent.MVar (newEmptyMVar, swapMVar)
import My.IO
import My.Prelude
import SpaceMiner.Types

data ConcurrentState = ConcurrentState
  { csEvents :: MVar [Event],
    csTexturePlacement :: MVar [TexturePlacements],
    csTexturePlacementTime :: MVar [(SystemTime, SystemTime)],
    csGameState :: MVar GameState,
    csGameLoopTime :: MVar [(SystemTime, SystemTime)],
    csRenderLoopTime :: MVar [(SystemTime, SystemTime)],
    csTotalLoopTime :: MVar [(SystemTime, SystemTime)]
  }

makeInitialConcurrentState :: GameState -> IO ConcurrentState
makeInitialConcurrentState gameState = do
  csEvents <- newMVar []
  csTexturePlacement <- newEmptyMVar
  csTexturePlacementTime <- newMVar []
  csGameState <- newMVar gameState
  csGameLoopTime <- newMVar []
  csRenderLoopTime <- newMVar []
  csTotalLoopTime <- newMVar []
  pure $ ConcurrentState {..}

fetchEvents :: ConcurrentState -> IO [Event]
fetchEvents ConcurrentState {csEvents} = do
  capturedInputs <- modifyMVar csEvents $ \cs -> pure ([], cs)
  time <- getSystemTime
  pure $ reverse $ GameLoopEvent time : capturedInputs

updateGameState :: ConcurrentState -> GameState -> IO ()
updateGameState ConcurrentState {csGameState} = void . swapMVar csGameState
