module Platformer.Main where

import Codec.Picture (readPng)
import My.IO
import My.Prelude
import Platformer.GameState
import Platformer.Visualize
import Playtime

gameDir :: FilePath
gameDir = "platformer"

main :: IO ()
main =
  playtimeLiveCode
    makeEngineConfig
    "Platformer.Main"
    "makeEngineConfig"
    (gameDir </> "Platformer")
    [ gameDir </> "Platformer/Main.hs",
      gameDir </> "Platformer/GameState.hs",
      gameDir </> "Platformer/Visualize.hs"
    ]

makeEngineConfig :: LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  let dim = Dimensions {width = 320, height = 240}
      loadTx = \(TextureId name) -> either fail pure =<< (readPng $ gameDir </> "assets" </> name)
      stepGameState textures es@EngineState {..} old_gs event = do
        let new_gs = stepGameStatePure textures old_gs es event
        saveMay es new_gs
        fromMaybe new_gs <$> loadMay es
  wireEngineConfig dim 1 all_textures liveCodeState stepGameState visualize loadTx $ makeInitialGameState dim

all_textures :: [TextureId]
all_textures = tuId <$> [main_character, floor_plate]
