module LiveCodingDemo.Main where

import Codec.Picture (readPng)
import LiveCodingDemo.GameState
import LiveCodingDemo.Visualize
import My.IO
import My.Prelude
import Playtime
import System.Random

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
      (stepGameState . textureDim textures)
      (visualize . textureSprites textures)
      loadTexture
      (snd . textures <$> allEnumValues)
  where
    dim = (1024, 768)
    initialGameState = makeInitialGameState dim <$> randomIO
    stepGameState area es@EngineState {..} old_gs event = do
      seed <- randomIO
      let new_gs = stepGameStatePure seed area old_gs es event
      if Key'R `setMember` esKeysPressed
        then initialGameState
        else pure new_gs
    loadTexture = \name -> either fail pure =<< (readPng $ gameDir </> "assets" </> name)
