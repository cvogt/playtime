module Playtime.Debug where

import Control.DeepSeq (rnf)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HMS
import Data.List (lines, zip)
import qualified Data.Text as T
import My.IO
import My.Prelude
import Playtime.ConcurrentState
import Playtime.EngineConfig
import Playtime.EngineState
import Playtime.LiveCode
import Playtime.Util
import System.Console.ANSI as ANSI
import qualified System.Console.Terminal.Size as TerminalSize

-- this modile is pretty messy. got to clean up some time.

-- this relies on lazy evaluation to be correct and measures the time to force a
trackTimeM :: NFData a => MVar [Double] -> IO a -> IO a
trackTimeM mvar action = do
  before <- getSystemTime
  res <- action
  rnf res & \() -> do
    after <- getSystemTime
    modifyMVar_ mvar $ pure . ((pico2Double $ timeDiffPico before after) :)
    pure res

trackTime :: NFData a => MVar [Double] -> a -> IO ()
trackTime mvar = void . trackTimeM mvar . pure

forkDebugTerminal :: ConcurrentState -> MVar EngineConfig -> Maybe LiveCodeState -> IO ThreadId
forkDebugTerminal ConcurrentState {..} engineConfigMVar lcsMay = do
  forkIO $ do
    flip iterateM_ (0, 0, 0, 0) $ \(oldAvgGameStepTime, oldAvgRenderGLTime, oldAvgTotalLoopTime, oldAvgVisualizeTime) -> do
      engineState@EngineState {..} <- readMVar csEngineState
      gameStepTimes <- modifyMVar csTimeStep $ \t -> pure ([], t)
      renderGLTimes <- modifyMVar csTimeGL $ \t -> pure ([], t)
      totalLoopTimes <- modifyMVar csTimeRender $ \t -> pure ([], t)
      visualizeTimes <- modifyMVar csTimeVisualize $ \t -> pure ([], t)
      let newAvgGameStepTime = if not $ null gameStepTimes then avg' gameStepTimes else oldAvgGameStepTime
          newAvgRenderGLTime = if not $ null renderGLTimes then avg' renderGLTimes else oldAvgRenderGLTime
          newAvgTotalLoopTime = if not $ null totalLoopTimes then avg' totalLoopTimes else oldAvgTotalLoopTime
          newAvgVisualizeTime = if not $ null visualizeTimes then avg' visualizeTimes else oldAvgVisualizeTime
          display =
            [ "fps: " <> show (1 / newAvgTotalLoopTime),
              "totalLoopTime: " <> show newAvgTotalLoopTime,
              "visualizeTime: " <> show newAvgVisualizeTime,
              "renderGLTime: " <> show newAvgRenderGLTime,
              "gameStepTime: " <> show newAvgGameStepTime,
              repeat '-',
              "esCursorPos: " <> show esCursorPos,
              "esKeysPressed: " <> show esKeysPressed,
              "esMousePressed: " <> show esMousePressed,
              --"esActions: " <> show esActions,
              "esTimePassed: " <> show esTimePassed,
              repeat '-'
            ]

      gameDebugInfo <- ecGameDebugInfo <$> readMVar engineConfigMVar
      gameInfo <- gameDebugInfo engineState
      Just TerminalSize.Window {height, width} <- TerminalSize.size
      (join . join -> ce) <- sequence $ tryReadMVar . lcsCompileError <$> lcsMay

      let compileError = (maybe [] (\e -> lines $ setSGRCode [SetColor Foreground Vivid Red] <> e <> "\n" <> replicate (width -1) '-' <> setSGRCode [ANSI.Reset]) ce)
          output =
            T.unpack $ T.unlines $ take (height -2) $ join $
              T.chunksOf width . T.stripEnd . T.pack . take (width -1) <$> compileError <> display <> gameInfo

      putStrLn output
      cursorUp $ (length $ compileError <> display <> gameInfo) + 1 -- trailing newline
      threadDelay $ 200 * 1000 -- FIXME: changing this to 100 * make process freeze on exit
      cursorUp $ (length $ compileError <> display <> gameInfo) + 1 -- trailing newline
      setCursorColumn 0
      clearFromCursorToScreenEnd

      pure (oldAvgGameStepTime, newAvgRenderGLTime, newAvgTotalLoopTime, newAvgVisualizeTime)

debugPrint :: ToJSON a => a -> [[Char]]
debugPrint a = case toJSON a of
  Object hms -> fmap (\(k, v) -> T.unpack k <> ": " <> v) $ sortOn fst $ HMS.keys hms `zip` (enc <$> HMS.elems hms)
  other -> [enc other]
  where
    enc = take 200 . either show T.unpack . decodeUtf8' . BSL.toStrict . (\_ -> "foo") -- encode
