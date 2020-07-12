module SpaceMiner where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import Game
-- import Music

import Graphics
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Extra
import My.IO
import My.Prelude
import SpaceMiner.ConcurrentState
import SpaceMiner.Debug
import SpaceMiner.Debug.Vty
import SpaceMiner.GL
import SpaceMiner.GLFW
import SpaceMiner.Types

main :: IO ()
main = do
  -- basic configuration
  let dim = Dimensions {width = 320, height = 240} -- logical pixel resolution
  let scale = 3 -- scale up to screen resolution

  -- initialization
  igs <- makeInitialGameState scale dim <$> getSystemTime
  cs@ConcurrentState {csTotalLoopTime, csRenderLoopTime, csSpritePlacementTime} <- makeInitialConcurrentState igs

  --void $ forkIO $ forever $ playMusic
  void $ forkDebugTerminal cs

  -- open gl rendering loop
  withGLFW (gsWindowSize $ fst igs) "SpaceMiner" $ \window -> do
    textures <- loadTextures
    setEventCallback window $ void . handleEvent cs
    whileM $ trackTime csTotalLoopTime $ do
      GLFW.pollEvents
      gs <- handleEvent cs . RenderEvent =<< getSystemTime
      pure gs
        >>= trackTime csSpritePlacementTime . pure . computeSpritePlacements textures
        >>= trackTime csRenderLoopTime . renderGL textures window
      pure $ not $ gameExitRequested gs
  where
    handleEvent :: ConcurrentState -> Event -> IO GameState
    handleEvent ConcurrentState {csGameState, csGameLoopTime} event = do
      modifyMVar csGameState $ \oldGameState -> do
        new_gs <- trackTime csGameLoopTime $ pure $ applyEventToGameState event oldGameState
        saveMay new_gs
        dupe . fromMaybe new_gs <$> loadMay new_gs
      where
        saveLocation = $(makeRelativeToProject "savegame.json" >>= strToExp)
        saveMay gameState = do
          let GenericGameState {gsActions} = get gameState
              (persistentGameState@PersistentGameState {}) = get gameState
          when (OneTimeEffect Save `setMember` gsActions) $ writeFile saveLocation $ BSL.toStrict $ encode $ persistentGameState
        loadMay gameState = do
          let GenericGameState {gsActions} = get gameState
          if OneTimeEffect Load `setMember` gsActions
            then do
              npgs@PersistentGameState {} <- either fail pure . eitherDecode . BSL.fromStrict =<< readFile saveLocation
              pure $ Just $ update gameState $ \_ -> npgs
            else pure Nothing
