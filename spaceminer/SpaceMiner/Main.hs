module Main where

import Codec.Picture (readPng)
import My.IO
import My.Prelude
import Playtime
import SpaceMiner.GameState
import SpaceMiner.Visualize

dim :: Dimensions
dim = Dimensions {width = 320, height = 240}

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
    stepGameState
    visualize
    loadTexture
    (snd . textures <$> allEnumValues)
    $ makeInitialGameState dim
  where
    stepGameState loadedTextures es@EngineState {..} old_gs event = do
      let new_gs = stepGameStatePure loadedTextures old_gs es event
      saveMay es new_gs
      fromMaybe new_gs <$> loadMay es
    loadTexture = \name -> either fail pure =<< (readPng $ gameDir </> "assets" </> name)
