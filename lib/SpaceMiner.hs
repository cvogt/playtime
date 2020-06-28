module SpaceMiner where

import Control.Concurrent.MVar (putMVar, takeMVar)
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
  cs@ConcurrentState {csTexturePlacement} <- makeInitialConcurrentState igs

  --void $ forkIO $ forever $ playMusic
  void $ forkDebugTerminal cs

  -- game state loop
  void $ forkIO $ do
    flip unfoldM_ igs $ \ogs -> do
      events <- fetchEvents cs
      ngs <- trackGameLoopTime cs $ foldl handleEvent ogs events
      updateGameState cs ngs
      -- putMVar here leads to about 1 frame input lag, because after the mvar becomes free the game loop runs immediately and then waits (not processing events) until the much slower rendering loop processed the frame
      putMVar csTexturePlacement =<< trackTexturePlacementTime cs (placeTextures ngs)
      maybeExitGameLoop ngs
    exitSuccess

  -- open gl rendering loop
  withGLFW logicDim scale "SpaceMiner" $ \window -> do
    textures <- loadTextures
    startCaptureEvents window logicDim cs
    forever $ trackTotalLoopTime cs $ do
      GLFW.pollEvents -- before takeMVar frees game loop for another run
      takeMVar csTexturePlacement >>= trackRenderLoopTime cs . renderGame textures window logicDim
