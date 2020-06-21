module SpaceMiner.Debug.Vty where

import qualified Data.Set as Set
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
          CursorPos (x', y') = gsMainCharacterPosition gameState
      update vty $ picForImage $ foldl1 (<->) $
        string (defAttr `withForeColor` white)
          <$> [ "fps: " <> show newAvRenderLoopTime,
                "1/gameLoopTime: " <> show newAvgGameLoopTime,
                "opengl pos: " <> show (x, y),
                "main char: " <> show (x', y'),
                "sprite count: " <> show (Set.size $ gsBoard gameState)
              ]

      threadDelay $ 100 * 1000
      pure (newAvgGameLoopTime, newAvRenderLoopTime)
