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

textures :: TextureId -> (Scale, FilePath)
textures = \case
  Plane -> (1, "plane.png")
  Enemy -> (0.1, "enemy_red.png")
  Heart -> (0.025, "haskell_love_logo.png")

main :: IO ()
main =
  playtime . Left
    =<< makeLiveCodeState makeEngineConfig "LiveCodingDemo.Main" "makeEngineConfig" (gameDir </> "LiveCodingDemo")

makeEngineConfig :: Maybe LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  initialGameState
    >>= wireEngineConfig
      dim
      1
      liveCodeState
      (stepGameState . textureArea textures)
      (visualize . textureSprites textures)
      loadTexture
      (snd . textures <$> allEnumValues)
  where
    initialGameState = makeInitialGameState dim <$> randomIO
    stepGameState area es@EngineState {..} old_gs event = do
      pre <- preIO
      let new_gs = stepGameStatePure pre area old_gs es event
      postIO es new_gs
    preIO :: IO Int = randomIO
    postIO es new_gs = do
      post_gs <-
        if Key'R `setMember` esKeysPressed es
          then initialGameState
          else pure new_gs
      saveMay es post_gs
      fromMaybe post_gs <$> loadMay es
    loadTexture = \name -> either fail pure =<< (readPng $ gameDir </> "assets" </> name)
