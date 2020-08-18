module SpaceMiner.Main where

import Codec.Picture (readPng)
import My.IO
import My.Prelude
import Playtime
import SpaceMiner.GameState
import SpaceMiner.Visualize

gameDir :: FilePath
gameDir = "spaceminer"

main :: IO ()
main =
  playtime . Left
    =<< makeLiveCodeState makeEngineConfig "SpaceMiner.Main" "makeEngineConfig" (gameDir </> "SpaceMiner")

makeEngineConfig :: Maybe LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  wireEngineConfig
    (pure . makeInitialGameState dimensions . textureDim textures)
    (stepGameState . textureDim textures)
    (visualize . textureSprites textures)
    dimensions
    3
    liveCodeState
    loadTexture
    (snd . textures <$> allEnumValues)
  where
    dimensions = (320, 240)
    stepGameState tDim es@EngineState {..} old_gs event = do
      let new_gs = stepGameStatePure tDim old_gs es event
      let final_gs =
            if Key'R `setMember` esKeysPressed
              then makeInitialGameState dimensions tDim
              else new_gs
      saveMay es final_gs
      fromMaybe final_gs <$> loadMay es
    loadTexture = \name -> either fail pure =<< (readPng $ gameDir </> "assets" </> name)
