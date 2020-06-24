module SpaceMiner.MutableState where

import My.IO
import My.Prelude
import SpaceMiner.Types

data MutableState = MutableState
  { msEventsMVar :: MVar [Event]
  }

initialMutableState :: IO MutableState
initialMutableState = do
  msEventsMVar <- newMVar []
  pure $ MutableState {..}

fetchEvents :: MutableState -> IO [Event]
fetchEvents MutableState {msEventsMVar} = do
  capturedInputs <- modifyMVar msEventsMVar $ \cs -> pure ([], cs)
  time <- getSystemTime
  pure $ reverse $ GameLoopEvent time : capturedInputs
