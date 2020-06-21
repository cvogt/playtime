module SpaceMiner.Debug.Vty where

import Graphics.Vty
import My.IO
import My.Prelude

forkDebugTerminal :: IO ThreadId
forkDebugTerminal = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  flip forkFinally (\_ -> shutdown vty) $ do
    forever $ do
      time <- getSystemTime
      let line0 = string (defAttr `withForeColor` green) $ show time
          line1 = string (defAttr `withForeColor` green) ""
          img = line0 <-> line1
          pic2 = picForImage img
      update vty pic2
      threadDelay $ 500 * 1000
