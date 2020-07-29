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

srcFiles :: [FilePath]
srcFiles =
  [ gameDir </> "SpaceMiner/Main.hs",
    gameDir </> "SpaceMiner/GameState.hs",
    gameDir </> "SpaceMiner/Visualize.hs"
  ]

main :: IO ()
main =
  playtimeLiveCode makeEngineConfig "SpaceMiner.Main" "makeEngineConfig" (gameDir </> "SpaceMiner") srcFiles

makeEngineConfig :: LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  let loadTx = \(tuId . textureUse -> TextureFile name) -> either fail pure =<< (readPng $ gameDir </> "assets" </> name)
      stepGameState textures es@EngineState {..} old_gs event = do
        let new_gs = stepGameStatePure textures old_gs es event
        saveMay es new_gs
        fromMaybe new_gs <$> loadMay es
  wireEngineConfig dim 3 liveCodeState stepGameState visualize loadTx $ makeInitialGameState dim
