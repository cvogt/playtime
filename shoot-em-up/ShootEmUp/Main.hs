module ShootEmUp.Main where

import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import My.IO
import My.Prelude
import Playtime
import SDL.Mixer
import ShootEmUp.GameState
import ShootEmUp.Visualize
import System.Random

gameDir :: FilePath
gameDir = "shoot-em-up"

srcFiles :: [FilePath]
srcFiles =
  [ gameDir </> "ShootEmUp/Main.hs",
    gameDir </> "ShootEmUp/GameState.hs",
    gameDir </> "ShootEmUp/Visualize.hs"
  ]

--   foldl (.) id (flap [applyToEngineState, applyToGameState] event')
main :: IO ()
main =
  playtimeLiveCode makeEngineConfig "ShootEmUp.Main" "makeEngineConfig" (gameDir </> "ShootEmUp") srcFiles

makeEngineConfig :: LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  let dim = Dimensions {width = 1024, height = 768} -- logical pixel resolution
  recoveredGameState <- startLiveCode liveCodeState
  gameStateMVar <- newMVar =<< maybe (makeInitialGameState dim) pure recoveredGameState

  popSound <- newEmptyMVar
  void $ forkIO $ do
    openAudio (Audio 44100 FormatS16_LSB Mono) 4410
    setChannels 500 -- default 8 seems to be problematic, so choosing somethign much larger ... 500
    putMVar popSound =<< load (gameDir </> "assets/bubble_pop.ogg") -- https://freesound.org/people/blue2107/sounds/59978/
    void $ playForever =<< load (gameDir </> "assets/venus_music.ogg") -- https://opengameart.org/content/nes-shooter-music-5-tracks-3-jingles
  pure $
    EngineConfig
      { ecDim = dim,
        ecScale = 1,
        ecVisualize = \tx es -> visualize tx es <$> readMVar gameStateMVar,
        ecStepGameState = \es@EngineState {..} event ->
          modifyMVar_ gameStateMVar $ \old_gs -> do
            pre <- sequence $ replicate 10 randomIO
            let new_gs = stepGameStatePure pre old_gs es event
            liveCodeSwitch liveCodeState new_gs
            when (Key'Space `elem` esKeysPressed) $ play =<< readMVar popSound
            saveMay es new_gs
            fromMaybe new_gs <$> loadMay es,
        ecCheckIfContinue = pure . not . gameExitRequested,
        ecGameDebugInfo = \EngineState {..} -> do
          GameState {..} <- readMVar gameStateMVar
          let Pos x' y' = gsMainCharacterPosition
          pure
            [ "main char: " <> show (x', y'),
              "bullets: " <> show gsBullets,
              "enemies: " <> (show $ take 5 gsEnemies),
              "stars: " <> (show $ take 5 gsStars)
            ]
      }

tests :: IO ()
tests = pure ()
