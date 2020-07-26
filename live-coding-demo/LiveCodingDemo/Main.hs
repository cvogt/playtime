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
    gameDir </> "LiveCodingDemo/Graphics.hs"
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
            let new_gs = stepGameStatePure pre es old_gs event
            liveCodeSwitch liveCodeState new_gs
            pure $ new_gs,
        ecCheckIfContinue = pure . not . gameExitRequested,
        ecGameDebugInfo = \EngineState {..} -> do
          GameState {..} <- readMVar gameStateMVar
          pure
            [ "bullets: " <> show gsBullets,
              "stars: " <> show gsStars,
              "enemies: " <> show gsEnemies,
              "DEBUG: " <> show gsDebug
            ]
      }

tests :: IO ()
tests = pure ()
