module LiveCodingDemo.Main where

import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Concurrent.MVar (swapMVar)
import Data.Aeson (Result (..), Value, fromJSON, toJSON)
import LiveCodingDemo.GameState
import LiveCodingDemo.Graphics
import My.IO
import My.Prelude
import Playtime
import qualified Playtime.LiveCode
import System.FSNotify hiding (Event)
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
main = do
  engineConfigMVar <- newEmptyMVar
  putMVar engineConfigMVar =<< makeEngineConfig engineConfigMVar Nothing
  playtime engineConfigMVar

makeEngineConfig :: MVar EngineConfig -> Maybe Value -> IO EngineConfig
makeEngineConfig engineConfigMVar gameStateJson = do
  update <- newMVar False
  void $ forkIO $ do
    mgr <- startManagerConf $ WatchConfig DebounceDefault (100 * 1000) True
    -- start a watching job (in the background)
    void
      $ watchDir
        mgr
        (gameDir </> "LiveCodingDemo")
        (\_ -> True)
      $ \_ -> void $ swapMVar update True
    forever $ threadDelay $ 1000 * 1000
  let dim = Dimensions {width = 1024, height = 768} -- logical pixel resolution
  gameStateMVar <- (newMVar =<<) $ maybe (makeInitialGameState dim) pure $ (\case Error _ -> Nothing; Success gs -> Just gs {gsCompileErrors = Nothing}) . fromJSON =<< gameStateJson
  pure $
    EngineConfig
      { ecDim = dim,
        ecScale = 1,
        ecComputeSpritePlacements' = \tx es -> computeSpritePlacements tx es <$> readMVar gameStateMVar,
        ecStepGameState = stepGameState engineConfigMVar update gameStateMVar,
        ecCheckIfContinue = pure . not . gameExitRequested,
        ecGameDebugInfo = \EngineState {..} -> do
          GameState {..} <- readMVar gameStateMVar
          pure
            [ "compile status: " <> fromMaybe "SUCCESS" gsCompileErrors,
              "--------------------------------------"
            ]
      }

tests :: IO ()
tests = pure ()

stepGameState :: MVar EngineConfig -> MVar Bool -> MVar GameState -> EngineState -> Event -> IO ()
stepGameState engineConfigMVar update gameStateMVar es@EngineState {..} event = do
  modifyMVar_ gameStateMVar $ \old_gs -> do
    pre <- sequence $ replicate 10 randomIO

    let new_gs = stepGameStatePure pre es old_gs event

    needsUpdate <- readMVar update
    compileErrors <-
      if not needsUpdate
        then pure $ gsCompileErrors new_gs
        else do
          void $ swapMVar update False
          Playtime.LiveCode.compileAndEval srcFiles "LiveCodingDemo.Main" "makeEngineConfig" >>= \case
            Left err -> pure $ Just err
            Right makeEngineConfig' -> do
              void $ swapMVar engineConfigMVar =<< makeEngineConfig' engineConfigMVar (Just $ toJSON new_gs)
              pure Nothing
    pure $ new_gs {gsCompileErrors = compileErrors} -- doesn't clear compile errors because EngineConfig has already been replaced
