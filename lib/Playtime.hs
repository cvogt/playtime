module Playtime
  ( module Playtime,
    module Playtime.ConcurrentState,
    module Playtime.EngineState,
    GLFW.Key (..),
  )
where

import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Extra
import My.IO
import My.Prelude
import Playtime.ConcurrentState
import Playtime.Debug
import Playtime.Debug.Vty
import Playtime.EngineState
import Playtime.GL
import Playtime.GLFW
import Playtime.Types

-- README
-- Acronyms to know:
-- es = engine state
-- gs = game state
-- cs = concurrent state
-- pos = Position
-- dim = Dimension

playtime :: MVar EngineConfig -> IO ()
playtime ecMVar = do
  EngineConfig {ecScale, ecDim, ecCheckIfContinue} <- readMVar ecMVar
  -- initialization
  ies@EngineState {gsWindowSize} <- makeInitialEngineState ecScale ecDim <$> getSystemTime
  cs@ConcurrentState {..} <- makeInitialConcurrentState ies

  when False $ void $ forkDebugTerminal cs ecMVar

  -- open gl rendering loop
  withGLFW gsWindowSize "Playtime" $ \window -> do
    textures <- loadTextures
    setEventCallback window $ void . stepStates cs

    whileM $ trackTimeM csTimeRender $ do
      GLFW.pollEvents
      EngineConfig {ecComputeSpritePlacements'} <- readMVar ecMVar
      es <- stepStates cs . RenderEvent =<< getSystemTime
      pure es
        >>= trackTimeM csSpritePlacementTime . ecComputeSpritePlacements' textures
        >>= trackTimeM csTimeGL . renderGL textures window
      ecCheckIfContinue es
  where
    stepStates :: ConcurrentState -> Event -> IO EngineState
    stepStates ConcurrentState {..} event =
      modifyMVar csEngineState $ \old_es ->
        trackTimeM csTimeStep $ do
          EngineConfig {ecStepGameState} <- readMVar ecMVar
          let new_es = stepEngineState old_es event
          ecStepGameState new_es event
          pure (new_es, new_es)
