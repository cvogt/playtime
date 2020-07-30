module ShootEmUp.Main where

import Codec.Picture (readPng)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import My.Extra
import My.IO
import My.Prelude
import Playtime
import SDL.Mixer
import ShootEmUp.GameState
import ShootEmUp.Visualize
import System.Random

dim :: Dimensions
dim = Dimensions {width = 1024, height = 768}

gameDir :: FilePath
gameDir = "shoot-em-up"

main :: IO ()
main = do
  void $ forkIO $ do
    openAudio (Audio 44100 FormatS16_LSB Mono) 4410
    setChannels 500 -- default 8 seems to be problematic, so choosing somethign much larger ... 500
    setVolume 8 AllChannels
    void $ playForever =<< load (gameDir </> "assets/venus_music.ogg") -- https://opengameart.org/content/nes-shooter-music-5-tracks-3-jingles
  playtime . Left
    =<< makeLiveCodeState makeEngineConfig "ShootEmUp.Main" "makeEngineConfig" (gameDir </> "ShootEmUp")

makeEngineConfig :: Maybe LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  popSound <- newEmptyMVar
  void $ forkIO
    $ whileM
    $ fmap isLeft
    $ try @SomeException
    $ putMVar popSound =<< load (gameDir </> "assets/bubble_pop.ogg") -- https://freesound.org/people/blue2107/sounds/59978/
  makeInitialGameState dim
    >>= wireEngineConfig
      dim
      1
      liveCodeState
      (stepGameState popSound . textureArea textures)
      (visualize . textureSprites textures)
      loadTx
      (snd . textures <$> allEnumValues)
  where
    loadTx = \name -> either fail pure =<< (readPng $ gameDir </> "assets" </> name)
    stepGameState popSound area es@EngineState {..} old_gs event = do
      pre <- preIO
      let new_gs = stepGameStatePure pre area old_gs es event
      postIO es new_gs popSound
    preIO = sequence $ replicate 10 randomIO
    postIO es new_gs popSound = do
      when (Key'Space `elem` esKeysPressed es) $ play =<< readMVar popSound
      post_gs <- if Key'R `setMember` esKeysPressed es then makeInitialGameState dim else pure new_gs
      saveMay es post_gs
      fromMaybe post_gs <$> loadMay es
