module Playtime.Debug where

import Control.DeepSeq (rnf)
import GHC.Float (int2Double)
import GHC.Real ((/), round)
import My.Extra
import My.IO
import My.Prelude
import Playtime.ConcurrentState
import Playtime.LiveCode
import Playtime.Types
import Playtime.Util
import System.Console.ANSI

-- this relies on lazy evaluation to be correct and measures the time to force a
trackTimeM :: NFData a => MVar [(SystemTime, SystemTime)] -> IO a -> IO a
trackTimeM mvar action = do
  before <- getSystemTime
  res <- action
  pure $ rnf res
  after <- getSystemTime
  modifyMVar_ mvar $ pure . ((before, after) :)
  pure res

trackTime :: NFData a => MVar [(SystemTime, SystemTime)] -> a -> IO ()
trackTime mvar = void . trackTimeM mvar . pure

forkDebugTerminal :: ConcurrentState -> MVar EngineConfig -> Maybe LiveCodeState -> IO ThreadId
forkDebugTerminal ConcurrentState {..} engineConfigMVar lcsMay = do
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
          Pos x y = esCursorPos
      gameDebugInfo <- ecGameDebugInfo <$> readMVar engineConfigMVar
      gameInfo <- gameDebugInfo engineState
      clearFromCursorToScreenBeginning
      restoreCursor
      saveCursor
      ce <- sequence $ readMVar . lcsCompileError <$> lcsMay
      traverse_ (putStrLn . take 400) $
        ( (maybe "" (("compile status: " <>) . fromMaybe "no errors") ce)
            :| [ "fps: " <> show newAvgTotalLoopTime,
                 "1/renderLoopTime: " <> show newAvgRenderLoopTime,
                 "1/texturePlacementTime: " <> show newAvgTexturePlacementTime,
                 "1/timeStep: " <> show newAvgTimeStep,
                 "opengl pos: " <> show (x, y),
                 "keys: " <> show esKeysPressed,
                 "esTimePassed: " <> show esTimePassed
               ]
              <> gameInfo
        )
      threadDelay $ 500 * 1000 -- FIXME: changing this to 100 * make process freeze on exit
      pure (oldAvgTimeStep, newAvgTexturePlacementTime, newAvgRenderLoopTime, newAvgTotalLoopTime)
