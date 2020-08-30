module MultiSpace.Main where

import Codec.Picture (readPng)
import Control.Concurrent.MVar (newEmptyMVar)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import MultiSpace.Game
import My.IO
import My.Prelude
import Network.Run.TCP
import Network.Socket.ByteString (recv, sendAll)
import Playtime
import System.Random

gameDir :: FilePath
gameDir = "multispace"

port :: [Char]
port = "12021"

main :: IO ()
main =
  playtime . Left
    =<< makeLiveCodeState makeEngineConfig "MultiSpace.Main" "makeEngineConfig" (gameDir </> "MultiSpace")

makeEngineConfig :: Maybe LiveCodeState -> IO EngineConfig
makeEngineConfig liveCodeState = do
  gameStateMVar <- newEmptyMVar
  texturesMVar <- newMVar mempty

  wireEngineConfig'
    gameStateMVar
    texturesMVar
    (initialGameState . textureDim textures)
    (stepGameState . textureDim textures)
    (visualize . textureSprites textures)
    dimensions
    1
    liveCodeState
    loadTexture
    allTextures
  where
    allTextures = (snd . textures <$> allEnumValues)
    dimensions = (1024, 768)
    initialGameState tDim = makeInitialGameState dimensions tDim <$> randomIO
    {--myStepGameStateWithMVar gameStateMVar texturesMVar es playerNum event = do
      modifyMVar_ gameStateMVar $ \old_gs -> do
        textures <- readTextures allTextures loadTexture texturesMVar
        new_gs <- (stepGameState . textureDim textures) textures es old_gs event
        for_ liveCodeState $ flip liveCodeSwitch new_gs
        pure new_gs
    --}
    stepGameState tDim es@EngineState {..} old_gs event = do
      seed <- randomIO
      let new_gs = stepGameStatePure seed tDim old_gs es event
      let talk s = do
            msg <- recv s 1024
            -- let playerNum = 1
            -- myStepGameStateWithMVar gameStateMVar texturesMVar es playerNum event
            unless (BS.null msg) $ do
              sendAll s msg
              talk s
      if Key'H `setMember` esKeysPressed
        then void $ forkIO $ runTCPServer Nothing port $ talk
        else pure ()
      if Key'C `setMember` esKeysPressed
        then void $ forkIO $ runTCPClient "localhost" port $ \s -> do
          sendAll s "Hello, world!"
          msg <- recv s 1024
          putStr "Received: "
          C8.putStrLn msg
        else pure ()
      if Key'R `setMember` esKeysPressed
        then initialGameState tDim
        else pure new_gs
    loadTexture = \name -> either fail pure =<< (readPng $ gameDir </> "assets" </> name)
