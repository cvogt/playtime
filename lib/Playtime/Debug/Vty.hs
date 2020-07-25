module Playtime.Debug.Vty where

import GHC.Float (int2Double)
import GHC.Real ((/), round)
import qualified Graphics.Vty as Vty
import My.Extra
import My.IO
import My.Prelude
import Playtime.ConcurrentState
import Playtime.Types
import Playtime.Util

forkDebugTerminal :: ConcurrentState gameState -> (EngineState -> gameState -> [[Char]]) -> IO ThreadId
forkDebugTerminal ConcurrentState {..} gameDebugInfo = do
  -- FIXME: cursor stays hidden after termination
  cfg <- Vty.standardIOConfig
  vty <- Vty.mkVty cfg
  flip forkFinally (\_ -> Vty.shutdown vty) $ do
    flip iterateM_ (0, 0, 0, 0) $ \(oldAvgTimeStep, oldAvgTexturePlacementTime, oldAvgRenderLoopTime, oldAvgTotalLoopTime) -> do
      (engineState@EngineState {..}, gameState) <- readMVar csGameState
      timeStep <- modifyMVar csTimeStep $ \t -> pure ([], t)
      texturePlacementTimes <- modifyMVar csSpritePlacementTime $ \t -> pure ([], t)
      renderLoopTimes <- modifyMVar csTimeGL $ \t -> pure ([], t)
      totalLoopTimes <- modifyMVar csTimeRender $ \t -> pure ([], t)
      let newAvgTimeStep = if not $ null timeStep then (/ 10) . int2Double . round @Double @Int $ 10 * 1 / (pico2second $ avg $ uncurry timeDiffPico <$> timeStep) else oldAvgTimeStep
          newAvgTexturePlacementTime = if not $ null texturePlacementTimes then (/ 10) . int2Double . round @Double @Int $ 10 * 1 / (pico2second $ avg $ uncurry timeDiffPico <$> texturePlacementTimes) else oldAvgTexturePlacementTime
          newAvgRenderLoopTime = if not $ null renderLoopTimes then (/ 10) . int2Double . round @Double @Int $ 10 * 1 / (pico2second $ avg $ uncurry timeDiffPico <$> renderLoopTimes) else oldAvgRenderLoopTime
          newAvgTotalLoopTime = if not $ null totalLoopTimes then (/ 10) . int2Double . round @Double @Int $ 10 * 1 / (pico2second $ avg $ uncurry timeDiffPico <$> totalLoopTimes) else oldAvgTotalLoopTime
          Pos x y = gsCursorPos
      Vty.update vty $ Vty.picForImage $ foldl1 (Vty.<->) $
        Vty.string (Vty.defAttr `Vty.withForeColor` Vty.white)
          <$> ( "fps: " <> show newAvgTotalLoopTime
                  :| [ "1/renderLoopTime: " <> show newAvgRenderLoopTime,
                       "1/texturePlacementTime: " <> show newAvgTexturePlacementTime,
                       "1/timeStep: " <> show newAvgTimeStep,
                       "opengl pos: " <> show (x, y),
                       "keys: " <> show gsKeysPressed,
                       "gsTimePassed: " <> show gsTimePassed
                     ]
                    <> gameDebugInfo engineState gameState
              )

      threadDelay $ 500 * 1000 -- FIXME: changing this to 100 * make process freeze on exit
      pure (oldAvgTimeStep, newAvgTexturePlacementTime, newAvgRenderLoopTime, newAvgTotalLoopTime)
