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

forkDebugTerminal :: MVar (GameState, [Integer]) -> MVar [Integer] -> MVar [Integer] -> IO ThreadId
forkDebugTerminal gameLoopDebugMVar renderLoopDebugMVar totalLoopDebugMVar = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  flip forkFinally (\_ -> shutdown vty) $ do
    flip iterateM_ (0, 0, 0) $ \(oldAvgGameLoopTime, oldAvgRenderLoopTime, oldAvgTotalLoopTime) -> do
      (GameState {gsCursorPos, gsMainCharacterPosition, gsBoard, gsLastPlacement}, gameLoopTimes) <- modifyMVar gameLoopDebugMVar $ \v@(gs, _) -> pure ((gs, []), v)
      renderLoopTimes <- modifyMVar renderLoopDebugMVar $ \t -> pure ([], t)
      totalLoopTimes <- modifyMVar totalLoopDebugMVar $ \t -> pure ([], t)
      let newAvgGameLoopTime = if not $ null gameLoopTimes then (/ 100) . int2Double . round @Double @Int $ 100 * 1 / (pico2second $ avg gameLoopTimes) else oldAvgGameLoopTime
          newAvgRenderLoopTime = if not $ null renderLoopTimes then (/ 100) . int2Double . round @Double @Int $ 100 * 1 / (pico2second $ avg renderLoopTimes) else oldAvgRenderLoopTime
          newAvgTotalLoopTime = if not $ null totalLoopTimes then (/ 100) . int2Double . round @Double @Int $ 100 * 1 / (pico2second $ avg totalLoopTimes) else oldAvgTotalLoopTime
          CursorPos (x, y) = gsCursorPos
          CursorPos (x', y') = gsMainCharacterPosition
      update vty $ picForImage $ foldl1 (<->) $
        string (defAttr `withForeColor` white)
          <$> [ "fps: " <> show newAvgTotalLoopTime,
                "1/renderLoopTime: " <> show newAvgRenderLoopTime,
                "1/gameLoopTime: " <> show newAvgGameLoopTime,
                "opengl pos: " <> show (x, y),
                "main char: " <> show (x', y'),
                "last places sprite location: " <> show gsLastPlacement,
                "sprite count: " <> show (Set.size gsBoard)
              ]

      threadDelay $ 500 * 1000 -- FIXME: changing this to 100 * make process freeze on exit
      pure (newAvgGameLoopTime, newAvgRenderLoopTime, newAvgTotalLoopTime)
