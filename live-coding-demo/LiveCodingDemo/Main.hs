module LiveCodingDemo.Main where

import Codec.Picture (readPng)
import LiveCodingDemo.GameState
import LiveCodingDemo.Visualize
import My.IO
import My.Prelude
import Playtime
import System.Random

dim :: Dim
dim = (1024, 768)

gameDir :: FilePath
gameDir = "live-coding-demo"

main :: IO ()
main =
  playtime . Left
    =<< makeLiveCodeState makeEngineConfig "LiveCodingDemo.Main" "makeEngineConfig" (gameDir </> "LiveCodingDemo")

makeEngineConfig :: Maybe LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  initial_gs
    >>= wireEngineConfig
      dim
      1
      liveCodeState
      (stepGameState . textureArea textures)
      (visualize . textureSprites textures)
      loadTexture
      (snd . textures <$> allEnumValues)
  where
    initial_gs = makeInitialGameState dim <$> randomIO
    stepGameState area es@EngineState {..} old_gs event = do
      pre <- preIO
      let new_gs = stepGameStatePure pre area old_gs es event
      postIO es new_gs
    preIO = sequence $ replicate 1500 randomIO
    postIO es new_gs = do
      post_gs <-
        if Key'R `setMember` esKeysPressed es
          then initial_gs
          else pure new_gs
      saveMay es post_gs
      fromMaybe post_gs <$> loadMay es
    loadTexture = \name -> either fail pure =<< (readPng $ gameDir </> "assets" </> name)
