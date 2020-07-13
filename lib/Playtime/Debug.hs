module Playtime.Debug where

import Control.DeepSeq (rnf)
import My.IO
import My.Prelude

-- this relies on lazy evaluation to be correct and measures the time to force a
trackTime :: NFData a => MVar [(SystemTime, SystemTime)] -> IO a -> IO a
trackTime mvar action = do
  before <- getSystemTime
  res <- action
  pure $ rnf res
  after <- getSystemTime
  modifyMVar_ mvar $ pure . ((before, after) :)
  pure res
