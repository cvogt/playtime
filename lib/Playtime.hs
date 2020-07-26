module Playtime
  ( module Playtime,
    module Playtime.EngineState,
    module Playtime.Geometry,
    module Playtime.SaveLoad,
    module Playtime.Textures,
    module Playtime.Types,
    module Playtime.Util,
    GLFW.Key (..),
    GLFW.KeyState (..),
    LiveCodeState,
    startLiveCode,
    liveCodeSwitch,
    debugPrint,
  )
where

import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Extra
import My.IO
import My.Prelude
import Playtime.ConcurrentState
import Playtime.Debug
import Playtime.EngineState
import Playtime.GL
import Playtime.GLFW
import Playtime.Geometry
import Playtime.LiveCode
import Playtime.SaveLoad
import Playtime.Textures
import Playtime.Types
import Playtime.Util

-- README
-- Acronyms to know:
-- es = engine state
-- gs = game state
-- cs = concurrent state
-- pos = Position
-- dim = Dimension

playtime :: MVar EngineConfig -> IO ()
playtime = playtime' Nothing

playtimeLiveCode :: (LiveCodeState -> IO EngineConfig) -> [Char] -> [Char] -> FilePath -> [FilePath] -> IO ()
playtimeLiveCode makeEngineConfig lcsModule lcsExpression lcsWatchDir lcsSrcFiles = do
  lcs <- makeLiveCodeState makeEngineConfig lcsModule lcsExpression lcsWatchDir lcsSrcFiles
  playtime' (Just lcs) $ lcsEngineConfig lcs

playtime' :: Maybe LiveCodeState -> MVar EngineConfig -> IO ()
playtime' lcsMay ecMVar = do
  EngineConfig {ecScale, ecDim, ecCheckIfContinue} <- readMVar ecMVar
  -- initialization
  ies@EngineState {esWindowSize} <- makeInitialEngineState ecScale ecDim <$> getSystemTime
  cs@ConcurrentState {..} <- makeInitialConcurrentState ies

  void $ forkDebugTerminal cs ecMVar lcsMay

  -- open gl rendering loop
  withGLFW esWindowSize "Playtime" $ \window -> do
    textures <- loadTextures
    setEventCallback window $ void . stepStates cs

    whileM $ trackTimeM csTimeRender $ do
      GLFW.pollEvents
      EngineConfig {ecVisualize} <- readMVar ecMVar
      es <- stepStates cs . RenderEvent =<< getSystemTime
      pure es
        >>= trackTimeM csSpritePlacementTime . ecVisualize textures
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
