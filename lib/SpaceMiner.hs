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
  let logicDim = Dimensions {width = 320, height = 240}
  let scale = 3 :: Scale

  -- initialization
  igs <- makeInitialGameState logicDim <$> getSystemTime
  cs@ConcurrentState {csTotalLoopTime, csRenderLoopTime, csSpritePlacementTime} <- makeInitialConcurrentState igs

  --void $ forkIO $ forever $ playMusic
  void $ forkDebugTerminal cs

  -- open gl rendering loop
  withGLFW logicDim scale "SpaceMiner" $ \window -> do
    textures <- loadTextures
    setEventCallback window logicDim $ void . handleEvent cs
    whileM $ trackTime csTotalLoopTime $ do
      GLFW.pollEvents
      gs <- handleEvent cs . RenderEvent =<< getSystemTime
      pure gs
        >>= trackTime csSpritePlacementTime . pure . computeSpritePlacements
        >>= trackTime csRenderLoopTime . renderGL textures window logicDim
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
        saveMay (GameState GenericGameState {gsActions} _ persistentGameState) =
          when (OneTimeEffect Save `setMember` gsActions) $ writeFile saveLocation $ BSL.toStrict $ encode $ persistentGameState
        loadMay gameState@(GameState GenericGameState {gsActions} _ _) =
          if OneTimeEffect Load `setMember` gsActions
            then do
              npgs <- either fail pure . eitherDecode . BSL.fromStrict =<< readFile saveLocation
              pure $ Just gameState {gsPersistentGameState = npgs}
            else pure Nothing
