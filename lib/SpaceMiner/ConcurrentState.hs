module SpaceMiner.ConcurrentState where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, swapMVar, takeMVar)
import Graphics
import My.IO
import My.Prelude
import SpaceMiner.Debug
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

sendGameState :: ConcurrentState -> GameState -> IO ()
sendGameState cs@ConcurrentState {csGameState, csGameLoopTime} gs = do
  void $ trackTime csGameLoopTime $ pure csGameState
  void $ swapMVar csGameState gs
  sendSpritePlacements cs

sendSpritePlacements :: ConcurrentState -> IO ()
sendSpritePlacements ConcurrentState {csTexturePlacement} = do
  -- putMVar here leads to about 1 frame input lag, because after the mvar becomes free the game loop runs immediately and then waits (not processing events) until the much slower rendering loop processed the frame
  putMVar csTexturePlacement []

receiveSpritePlacements :: ConcurrentState -> IO [TexturePlacements]
receiveSpritePlacements ConcurrentState {csGameState, csTexturePlacement, csTexturePlacementTime} = do
  void $ takeMVar csTexturePlacement
  gs <- readMVar csGameState
  let texturePlacements = computeSpritePlacements gs
  void $ trackTime csTexturePlacementTime $ pure texturePlacements
  pure texturePlacements
