module ShootEmUp.Main where

import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Concurrent.MVar (swapMVar)
import Data.Aeson (Result (..), Value, fromJSON, toJSON)
import My.IO
import My.Prelude
import Playtime
import qualified Playtime.LiveCode
import Playtime.SaveLoad
import SDL.Mixer
import ShootEmUp.GameState
import ShootEmUp.Graphics
import System.FSNotify hiding (Event)
import System.Random

gameDir :: FilePath
gameDir = "shoot-em-up"

srcFiles :: [FilePath]
srcFiles =
  [ gameDir </> "ShootEmUp/Main.hs",
    gameDir </> "ShootEmUp/GameState.hs",
    gameDir </> "ShootEmUp/Graphics.hs"
  ]

--   foldl (.) id (flap [applyToEngineState, applyToGameState] event')
main :: IO ()
main = do
  engineConfigMVar <- newEmptyMVar
  putMVar engineConfigMVar =<< makeEngineConfig engineConfigMVar Nothing
  playtime engineConfigMVar

makeEngineConfig :: MVar EngineConfig -> Maybe Value -> IO EngineConfig
makeEngineConfig engineConfigMVar gameStateJson = do
  popSound <- newEmptyMVar
  update <- newMVar False
  void $ forkIO $ do
    mgr <- startManagerConf $ WatchConfig DebounceDefault (100 * 1000) True
    -- start a watching job (in the background)
    void
      $ watchDir
        mgr
        (gameDir </> "ShootEmUp")
        (\_ -> True)
      $ \_ -> void $ swapMVar update True
    forever $ threadDelay $ 1000 * 1000
  void $ forkIO $ do
    openAudio (Audio 44100 FormatS16_LSB Mono) 4410
    setChannels 500 -- default 8 seems to be problematic, so choosing somethign much larger ... 500
    putMVar popSound =<< load (gameDir </> "assets/bubble_pop.ogg") -- https://freesound.org/people/blue2107/sounds/59978/
    void $ playForever =<< load (gameDir </> "assets/venus_music.ogg") -- https://opengameart.org/content/nes-shooter-music-5-tracks-3-jingles
  let dim = Dimensions {width = 1024, height = 768} -- logical pixel resolution
  gameStateMVar <- (newMVar =<<) $ maybe (makeInitialGameState dim) pure $ (\case Error _ -> Nothing; Success gs -> Just gs {gsCompileErrors = Nothing}) . fromJSON =<< gameStateJson
  pure $
    EngineConfig
      { ecDim = dim,
        ecScale = 1,
        ecComputeSpritePlacements' = \tx es -> computeSpritePlacements tx es <$> readMVar gameStateMVar,
        ecStepGameState = stepGameState engineConfigMVar popSound update gameStateMVar,
        ecCheckIfContinue = pure . not . gameExitRequested,
        ecGameDebugInfo = \EngineState {..} -> do
          GameState {..} <- readMVar gameStateMVar
          let Pos x' y' = gsMainCharacterPosition
          pure
            [ "main char: " <> show (x', y'),
              "bullets: " <> show gsBullets,
              "enemies: " <> (show $ take 5 gsEnemies),
              "stars: " <> (show $ take 5 gsStars),
              "compile status: " <> fromMaybe "SUCCESS" gsCompileErrors,
              "--------------------------------------"
            ]
      }

tests :: IO ()
tests = pure ()

stepGameState :: MVar EngineConfig -> MVar Chunk -> MVar Bool -> MVar GameState -> EngineState -> Event -> IO ()
stepGameState engineConfigMVar popSound update gameStateMVar es@EngineState {..} event = do
  modifyMVar_ gameStateMVar $ \old_gs -> do
    pre <- sequence $ replicate 10 randomIO

    let new_gs = stepGameStatePure pre es old_gs event

    when (Key'Space `elem` gsKeysPressed) $ play =<< readMVar popSound

    saveMay es new_gs
    final_gs <- fromMaybe new_gs <$> loadMay es

    needsUpdate <- readMVar update
    compileErrors <-
      if not needsUpdate
        then pure $ gsCompileErrors new_gs
        else do
          void $ swapMVar update False
          Playtime.LiveCode.compileAndEval srcFiles "ShootEmUp.Main" "makeEngineConfig" >>= \case
            Left err -> pure $ Just err
            Right makeEngineConfig' -> do
              void $ swapMVar engineConfigMVar =<< makeEngineConfig' engineConfigMVar (Just $ toJSON new_gs)
              pure Nothing
    pure $ final_gs {gsCompileErrors = compileErrors} -- doesn't clear compile errors because EngineConfig has already been replaced
