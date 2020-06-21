module SpaceMiner.Debug.Vty where

import GHC.Float (int2Double)
import GHC.Real ((/), round)
import GLFWHelpers
import Game
import Graphics.Vty
import My.Extra
import My.IO
import My.Prelude
import SpaceMiner.Util

forkDebugTerminal :: MVar (GameState, [Integer]) -> MVar [Integer] -> IO ThreadId
forkDebugTerminal gameLoopDebugMVar renderLoopDebugMVar = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  flip forkFinally (\_ -> shutdown vty) $ do
    flip iterateM_ (0, 0) $ \(oldAvgGameLoopTime, oldAvgRenderLoopTime) -> do
      (gameState, gameLoopTimes) <- modifyMVar gameLoopDebugMVar $ \v@(gs, _) -> pure ((gs, []), v)
      renderLoopTimes <- modifyMVar renderLoopDebugMVar $ \t -> pure ([], t)
      let newAvgGameLoopTime = if not $ null gameLoopTimes then (/ 100) . int2Double . round @Double @Int $ 100 * 1 / (pico2second $ avg gameLoopTimes) else oldAvgGameLoopTime
          newAvRenderLoopTime = if not $ null renderLoopTimes then (/ 100) . int2Double . round @Double @Int $ 100 * 1 / (pico2second $ avg renderLoopTimes) else oldAvgRenderLoopTime
          CursorPos (x, y) = gsCursorPos gameState
      update vty $ picForImage $
        foldl1
          (<->)
          [ string (defAttr `withForeColor` white) $ "fps: " <> show newAvRenderLoopTime,
            string (defAttr `withForeColor` white) $ "1/gameLoopTime: " <> show newAvgGameLoopTime,
            string (defAttr `withForeColor` white) $ "opengl pos: " <> show (x, y)
          ]

      threadDelay $ 500 * 1000
      pure (newAvgGameLoopTime, newAvRenderLoopTime)
