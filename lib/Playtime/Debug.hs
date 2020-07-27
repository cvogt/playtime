module Playtime.Debug where

import Control.DeepSeq (rnf)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HMS
import Data.List (lines, zip)
import qualified Data.List as DL
import qualified Data.Text as T
import GHC.Float (int2Double)
import GHC.Real ((/), round)
import My.Extra
import My.IO
import My.Prelude
import Playtime.ConcurrentState
import Playtime.LiveCode
import Playtime.Types
import Playtime.Util
import System.Console.ANSI as ANSI
import qualified System.Console.Terminal.Size as TerminalSize

-- this modile is pretty messy. got to clean up some time.

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
          display =
            [ "fps: " <> show newAvgTotalLoopTime,
              "1/renderLoopTime: " <> show newAvgRenderLoopTime,
              "1/texturePlacementTime: " <> show newAvgTexturePlacementTime,
              "1/timeStep: " <> show newAvgTimeStep,
              DL.repeat '-',
              "esCursorPos: " <> show esCursorPos,
              "esKeysPressed: " <> show esKeysPressed,
              "esMousePressed: " <> show esMousePressed,
              "esActions: " <> show esActions,
              "esTimePassed: " <> show esTimePassed,
              DL.repeat '-'
            ]

      gameDebugInfo <- ecGameDebugInfo <$> readMVar engineConfigMVar
      gameInfo <- gameDebugInfo engineState

      (join . join -> ce) <- sequence $ tryReadMVar . lcsCompileError <$> lcsMay
      Just TerminalSize.Window {height, width} <- TerminalSize.size
      putStrLn $ T.unpack $ T.unlines $ take (height -2) $ join $
        T.chunksOf width . T.stripEnd . T.pack
          <$> (maybe [] (\e -> lines $ setSGRCode [SetColor Foreground Vivid Red] <> e <> "\n" <> replicate (width -1) '-' <> setSGRCode [ANSI.Reset]) ce)
          <> (take (width -1) <$> display <> gameInfo)
      cursorUp $ (length $ display <> gameInfo) + 1 -- trailing newline
      setCursorColumn 0
      threadDelay $ 200 * 1000 -- FIXME: changing this to 100 * make process freeze on exit
      clearFromCursorToScreenEnd

      pure (oldAvgTimeStep, newAvgTexturePlacementTime, newAvgRenderLoopTime, newAvgTotalLoopTime)

debugPrint :: ToJSON a => a -> [[Char]]
debugPrint a = case toJSON a of
  Object hms -> fmap (\(k, v) -> T.unpack k <> ": " <> v) $ sortOn fst $ HMS.keys hms `zip` (enc <$> HMS.elems hms)
  other -> [enc other]
  where
    enc = take 200 . either show T.unpack . decodeUtf8' . BSL.toStrict . encode
