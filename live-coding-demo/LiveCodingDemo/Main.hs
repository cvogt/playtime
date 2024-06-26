module LiveCodingDemo.Main where

import Codec.Picture (readPng)
import LiveCodingDemo.Game
import My.IO
import My.Prelude
import Playtime
import System.Random

gameDir :: FilePath
gameDir = "live-coding-demo"

main :: IO ()
main =
  playtime . Left
    =<< makeLiveCodeState makeEngineConfig "LiveCodingDemo.Main" "makeEngineConfig" (gameDir </> "LiveCodingDemo")

makeEngineConfig :: Maybe LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  wireEngineConfig
    (initialGameState . textureDim textures)
    (stepGameState . textureDim textures)
    (visualize . textureSprites textures)
    dimensions
    1
    liveCodeState
    loadTexture
    (snd . textures <$> allEnumValues)
  where
    dimensions = (1024, 768)
    initialGameState :: (TextureId -> Dim) -> IO GameState
    initialGameState tDim = makeInitialGameState dimensions tDim <$> randomIO
    stepGameState tDim es@EngineState {..} old_gs event = do
      seed <- randomIO
      let new_gs = stepGameStatePure seed tDim old_gs es event
      if Key'R `setMember` esKeysPressed
        then initialGameState tDim
        else pure new_gs
    loadTexture = \name -> either fail pure =<< (readPng $ gameDir </> "assets" </> name)
