module Playtime.Debug.Vty where

import GHC.Float (int2Double)
import GHC.Real ((/), round)
import My.Extra
import My.IO
import My.Prelude
import Playtime.ConcurrentState
import Playtime.Types
import Playtime.Util
import System.Console.ANSI

forkDebugTerminal :: ConcurrentState -> MVar EngineConfig -> IO ThreadId
forkDebugTerminal ConcurrentState {..} engineConfigMVar = do
  -- FIXME: cursor stays hidden after termination
  forkIO $ do
    flip iterateM_ (0, 0, 0, 0) $ \(oldAvgTimeStep, oldAvgTexturePlacementTime, oldAvgRenderLoopTime, oldAvgTotalLoopTime) -> do
      engineState@EngineState {..} <- readMVar csEngineState
      timeStep <- modifyMVar csTimeStep $ \t -> pure ([], t)
      texturePlacementTimes <- modifyMVar csSpritePlacementTime $ \t -> pure ([], t)
      renderLoopTimes <- modifyMVar csTimeGL $ \t -> pure ([], t)
      totalLoopTimes <- modifyMVar csTimeRender $ \t -> pure ([], t)
      let newAvgTimeStep = if not $ null timeStep then (/ 10) . int2Double . round @Double @Int $ 10 * 1 / (pico2second $ avg $ uncurry timeDiffPico <$> timeStep) else oldAvgTimeStep
          newAvgTexturePlacementTime = if not $ null texturePlacementTimes then (/ 10) . int2Double . round @Double @Int $ 10 * 1 / (pico2second $ avg $ uncurry timeDiffPico <$> texturePlacementTimes) else oldAvgTexturePlacementTime
          newAvgRenderLoopTime = if not $ null renderLoopTimes then (/ 10) . int2Double . round @Double @Int $ 10 * 1 / (pico2second $ avg $ uncurry timeDiffPico <$> renderLoopTimes) else oldAvgRenderLoopTime
          newAvgTotalLoopTime = if not $ null totalLoopTimes then (/ 10) . int2Double . round @Double @Int $ 10 * 1 / (pico2second $ avg $ uncurry timeDiffPico <$> totalLoopTimes) else oldAvgTotalLoopTime
          Pos x y = gsCursorPos
      gameDebugInfo <- ecGameDebugInfo <$> readMVar engineConfigMVar
      gameInfo <- gameDebugInfo engineState
      clearFromCursorToScreenBeginning
      restoreCursor
      saveCursor
      traverse_ (putStrLn . take 400) $
        ( "fps: " <> show newAvgTotalLoopTime
            :| [ "1/renderLoopTime: " <> show newAvgRenderLoopTime,
                 "1/texturePlacementTime: " <> show newAvgTexturePlacementTime,
                 "1/timeStep: " <> show newAvgTimeStep,
                 "opengl pos: " <> show (x, y),
                 "keys: " <> show gsKeysPressed,
                 "gsTimePassed: " <> show gsTimePassed
               ]
              <> gameInfo
        )
      threadDelay $ 500 * 1000 -- FIXME: changing this to 100 * make process freeze on exit
      pure (oldAvgTimeStep, newAvgTexturePlacementTime, newAvgRenderLoopTime, newAvgTotalLoopTime)
