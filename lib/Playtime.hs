module Playtime
  ( module Playtime,
    GLFW.Key (..),
  )
where

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

-- README
-- Acronyms to know:
-- es = engine state
-- gs = game state
-- cs = concurrent state
-- pos = Position
-- dim = Dimension

data EngineConfig gs pre = EngineConfig
  { initialGameState :: gs,
    dim :: Dimensions,
    scale :: Scale,
    --    sounds :: Set FilePath,
    computeSpritePlacements' :: (TextureId -> Texture) -> (EngineState, gs) -> (Dimensions, [TexturePlacements]),
    preStepIO :: EngineState -> IO pre,
    pureStep :: pre -> EngineState -> gs -> Event -> gs,
    postStepIO :: EngineState -> gs -> IO gs,
    gameDebugInfo :: EngineState -> gs -> [[Char]]
  }

playtime :: forall gs pre. (NFData gs) => EngineConfig gs pre -> IO ()
playtime EngineConfig {..} = do
  -- initialization
  ies@EngineState {gsWindowSize} <- makeInitialEngineState scale dim <$> getSystemTime
  cs@ConcurrentState {..} <- makeInitialConcurrentState ies initialGameState

  void $ forkDebugTerminal cs gameDebugInfo

  let stepStates :: Event -> IO (EngineState, gs)
      stepStates event = do
        modifyMVar csGameState $ \(old_es, old_gs) -> trackTimeM csTimeStep $ do
          let new_es = stepEngineState old_es event

          pre <- preStepIO new_es

          let new_gs = pureStep pre old_es old_gs event

          dupe . (new_es,) <$> postStepIO new_es new_gs

  -- open gl rendering loop
  withGLFW gsWindowSize "Playtime" $ \window -> do
    textures <- loadTextures
    setEventCallback window $ void . stepStates

    whileM $ trackTimeM csTimeRender $ do
      GLFW.pollEvents
      state@(es, _) <- stepStates . RenderEvent =<< getSystemTime
      pure state
        >>= trackTimeM csSpritePlacementTime . pure . computeSpritePlacements' textures
        >>= trackTimeM csTimeGL . renderGL textures window
      pure $ not $ gameExitRequested es
