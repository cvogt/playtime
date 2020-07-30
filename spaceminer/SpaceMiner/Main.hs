module SpaceMiner.Main where

import Codec.Picture (readPng)
import My.IO
import My.Prelude
import Playtime
import SpaceMiner.GameState
import SpaceMiner.Visualize

dim :: Dim
dim = (320, 240)

gameDir :: FilePath
gameDir = "spaceminer"

main :: IO ()
main =
  playtime . Left
    =<< makeLiveCodeState makeEngineConfig "SpaceMiner.Main" "makeEngineConfig" (gameDir </> "SpaceMiner")

makeEngineConfig :: Maybe LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  wireEngineConfig
    dim
    3
    liveCodeState
    (stepGameState . textureArea textures)
    (visualize . textureSprites textures)
    loadTexture
    (snd . textures <$> allEnumValues)
    $ makeInitialGameState dim
  where
    stepGameState area es@EngineState {..} old_gs event = do
      let new_gs = stepGameStatePure area old_gs es event
      saveMay es new_gs
      fromMaybe new_gs <$> loadMay es
    loadTexture = \name -> either fail pure =<< (readPng $ gameDir </> "assets" </> name)
