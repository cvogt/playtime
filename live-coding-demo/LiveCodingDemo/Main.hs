module LiveCodingDemo.Main where

import LiveCodingDemo.GameState
import LiveCodingDemo.Visualize
import My.IO
import My.Prelude
import Playtime
import System.Random

gameDir :: FilePath
gameDir = "live-coding-demo"

srcFiles :: [FilePath]
srcFiles =
  [ gameDir </> "LiveCodingDemo/Main.hs",
    gameDir </> "LiveCodingDemo/GameState.hs",
    gameDir </> "LiveCodingDemo/Visualize.hs"
  ]

--   foldl (.) id (flap [applyToEngineState, applyToGameState] event')
main :: IO ()
main =
  playtimeLiveCode makeEngineConfig "LiveCodingDemo.Main" "makeEngineConfig" (gameDir </> "LiveCodingDemo") srcFiles

makeEngineConfig :: LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  let dim = Dimensions {width = 1024, height = 768} -- logical pixel resolution
  recoveredGameState <- startLiveCode liveCodeState
  gameStateMVar <- newMVar =<< maybe (makeInitialGameState dim) pure recoveredGameState

  pure $
    EngineConfig
      { ecDim = dim,
        ecScale = 1,
        ecVisualize = \tx es -> visualize tx es <$> readMVar gameStateMVar,
        ecStepGameState = \es event ->
          modifyMVar_ gameStateMVar $ \old_gs -> do
            pre <- sequence $ replicate 1500 randomIO
            let new_gs = stepGameStatePure pre old_gs es event
            liveCodeSwitch liveCodeState new_gs
            saveMay es new_gs
            fromMaybe new_gs <$> loadMay es,
        ecCheckIfContinue = pure . not . gameExitRequested,
        ecGameDebugInfo = \EngineState {..} -> do
          gs <- readMVar gameStateMVar
          pure $ debugPrint gs
      }

tests :: IO ()
tests = pure ()
