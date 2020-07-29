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

main :: IO ()
main =
  playtime . Left
    =<< makeLiveCodeState makeEngineConfig "LiveCodingDemo.Main" "makeEngineConfig" (gameDir </> "LiveCodingDemo")

makeEngineConfig :: Maybe LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  let dim = Dimensions {width = 1024, height = 768}
  initialGameState <- makeInitialGameState dim
  let loadTx = \(tuId . textureUse -> TextureFile name) -> either fail pure =<< (readPng $ gameDir </> "assets" </> name)
      stepGameState textures es@EngineState {..} old_gs event = do
        pre <- sequence $ replicate 1500 randomIO
        let new_gs =
              if Key'R `setMember` esKeysPressed
                then initialGameState
                else stepGameStatePure pre textures old_gs es event
        saveMay es new_gs
        fromMaybe new_gs <$> loadMay es
  wireEngineConfig dim 1 liveCodeState stepGameState visualize loadTx initialGameState
