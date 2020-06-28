module SpaceMiner where

import Game
import Graphics
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
-- import Music
import My.Extra
import My.IO
import My.Prelude
import SpaceMiner.ConcurrentState
import SpaceMiner.Debug
import SpaceMiner.Debug.Vty
import SpaceMiner.GL
import SpaceMiner.GLFW
import SpaceMiner.Types
import System.Exit (exitSuccess)

main :: IO ()
main = do
  -- basic configuration
  let logicDim = Dimensions {width = 320, height = 240}
  let scale = ScaleInt 3

  -- initialization
  igs <- makeInitialGameState logicDim <$> getSystemTime
  cs@ConcurrentState {csTotalLoopTime, csRenderLoopTime} <- makeInitialConcurrentState igs

  --void $ forkIO $ forever $ playMusic
  void $ forkDebugTerminal cs

  -- game state loop
  void $ forkIO $ do
    flip unfoldM_ igs $ \ogs -> do
      events <- fetchEvents cs
      let ngs = foldl handleGameEvent ogs events
      sendGameState cs ngs
      sendSpritePlacements cs $ computeSpritePlacements ngs
      pure $ if gsExitGame ngs then Nothing else Just ngs
    exitSuccess

  -- open gl rendering loop
  withGLFW logicDim scale "SpaceMiner" $ \window -> do
    textures <- loadTextures
    startCaptureEvents window logicDim cs
    forever $ trackTime csTotalLoopTime $ do
      GLFW.pollEvents -- before takeMVar frees game loop for another run
      receiveSpritePlacements cs >>= trackTime csRenderLoopTime . renderGame textures window logicDim
