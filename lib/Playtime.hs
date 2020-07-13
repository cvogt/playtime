module Playtime where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
-- import Music

import Game
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Extra
import My.IO
import My.Prelude
import Playtime.ConcurrentState
import Playtime.Debug
import Playtime.Debug.Vty
import Playtime.GL
import Playtime.GLFW
import Playtime.Textures
import Playtime.Types

data EngineConfig gameState = EngineConfig
  { initialGameState :: gameState,
    dim :: Dimensions,
    stepGameState :: EngineState -> gameState -> Event -> gameState,
    computeSpritePlacements' :: (TextureId -> Texture) -> (EngineState, gameState) -> (Dimensions, [TexturePlacements]),
    gameDebugInfo :: gameState -> [[Char]]
  }

playtime :: forall a. (FromJSON a, ToJSON a, NFData a) => EngineConfig a -> IO ()
playtime EngineConfig {initialGameState, gameDebugInfo, dim, stepGameState, computeSpritePlacements'} = do
  -- basic configuration
  let scale = 3 -- scale up to screen resolution

  -- initialization
  ies <- makeInitialEngineState scale dim <$> getSystemTime
  let igs = (ies, initialGameState)
  cs@ConcurrentState {csTotalLoopTime, csRenderLoopTime, csSpritePlacementTime} <- makeInitialConcurrentState igs

  --void $ forkIO $ forever $ playMusic
  void $ forkDebugTerminal cs gameDebugInfo

  -- open gl rendering loop
  withGLFW (gsWindowSize $ fst igs) "Playtime" $ \window -> do
    textures <- loadTextures
    setEventCallback window $ void . handleEvent cs
    whileM $ trackTime csTotalLoopTime $ do
      GLFW.pollEvents
      gs@(es, _) <- handleEvent cs . RenderEvent =<< getSystemTime
      pure gs
        >>= trackTime csSpritePlacementTime . pure . computeSpritePlacements' textures
        >>= trackTime csRenderLoopTime . renderGL textures window
      pure $ not $ gameExitRequested es
  where
    handleEvent :: ConcurrentState a -> Event -> IO (EngineState, a)
    handleEvent ConcurrentState {csGameState, csEngineStateTime, csGameStateTime} event = do
      modifyMVar csGameState $ \(old_es, old_gs) -> do
        new_es <- trackTime csEngineStateTime $ pure $ stepEngineState old_es event
        new_gs <- trackTime csGameStateTime $ pure $ stepGameState old_es old_gs event
        saveMay new_es new_gs
        dupe . (new_es,) . fromMaybe new_gs <$> loadMay new_es
      where
        saveLocation = $(makeRelativeToProject "savegame.json" >>= strToExp)
        saveMay :: EngineState -> a -> IO ()
        saveMay es gs = do
          let EngineState {gsActions} = es
          when (OneTimeEffect Save `setMember` gsActions) $ writeFile saveLocation $ BSL.toStrict $ encode gs
        loadMay :: EngineState -> IO (Maybe a)
        loadMay es = do
          let EngineState {gsActions} = es
          if OneTimeEffect Load `setMember` gsActions
            then do
              either fail pure . eitherDecode . BSL.fromStrict =<< readFile saveLocation
            else pure Nothing
