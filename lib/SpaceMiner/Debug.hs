module SpaceMiner.Debug where

import Control.DeepSeq (rnf)
import My.IO
import My.Prelude
import SpaceMiner.ConcurrentState

trackGameLoopTime :: NFData a => ConcurrentState -> a -> IO a
trackGameLoopTime = trackTime . csGameLoopTime

trackTexturePlacementTime :: NFData a => ConcurrentState -> a -> IO a
trackTexturePlacementTime = trackTime . csTexturePlacementTime

trackRenderLoopTime :: NFData a => ConcurrentState -> IO a -> IO a
trackRenderLoopTime = trackTimeM . csRenderLoopTime

trackTotalLoopTime :: NFData a => ConcurrentState -> IO a -> IO a
trackTotalLoopTime = trackTimeM . csTotalLoopTime

trackTime :: NFData a => MVar [(SystemTime, SystemTime)] -> a -> IO a
trackTime mvar = trackTimeM mvar . pure

trackTimeM :: NFData a => MVar [(SystemTime, SystemTime)] -> IO a -> IO a
trackTimeM mvar action = do
  before <- getSystemTime
  res <- action
  pure $ rnf res
  after <- getSystemTime
  modifyMVar_ mvar $ pure . ((before, after) :)
  pure res
