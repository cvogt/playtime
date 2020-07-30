module Platformer.Main where

import Codec.Picture (readPng)
import My.IO
import My.Prelude
import Platformer.GameState
import Platformer.Visualize
import Playtime

dim :: Dimensions
dim = Dimensions {width = 320, height = 240}

gameDir :: FilePath
gameDir = "platformer"

main :: IO ()
main =
  playtime . Left
    =<< makeLiveCodeState makeEngineConfig "Platformer.Main" "makeEngineConfig" (gameDir </> "Platformer")

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
