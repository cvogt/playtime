module ShootEmUp.Main where

import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import My.IO
import My.Prelude
import Playtime
import Playtime.SaveLoad
import Playtime.Types
import SDL.Mixer
import ShootEmUp.GameState
import ShootEmUp.Graphics
import System.Random

gameDir :: FilePath
gameDir = "shoot-em-up" -- $ (makeRelativeToProject "shoot-em-up" >>= strToExp)

--   foldl (.) id (flap [applyToEngineState, applyToGameState] event')
main :: IO ()
main = do
  popSound <- newEmptyMVar
  void $ forkIO $ do
    openAudio (Audio 44100 FormatS16_LSB Mono) 4410
    setChannels 500 -- default 8 seems to be problematic, so choosing somethign much larger ... 500
    putMVar popSound =<< load (gameDir </> "assets/bubble_pop.ogg") -- https://freesound.org/people/blue2107/sounds/59978/
    void $ playForever =<< load (gameDir </> "assets/venus_music.ogg") -- https://opengameart.org/content/nes-shooter-music-5-tracks-3-jingles
  let dim = Dimensions {width = 1024, height = 768} -- logical pixel resolution
      postStepIO es@EngineState {..} gs@GameState {..} = do
        when (Key'Space `elem` gsKeysPressed) $ play =<< readMVar popSound
        saveMay es gs
        fromMaybe gs <$> loadMay es
  igs <- makeInitialGameState dim
  playtime $ EngineConfig igs dim 1 computeSpritePlacements preStepIO stepGameState' postStepIO debug
  where
    preStepIO _ = sequence $ take 10 $ toList $ repeat $ randomIO
    debug EngineState {..} GameState {..} =
      let Pos x' y' = gsMainCharacterPosition
       in [ "main char: " <> show (x', y'),
            "bullets: " <> show gsBullets,
            "enemies: " <> show gsEnemies,
            "stars: " <> show gsStars
          ]

tests :: IO ()
tests = pure ()
