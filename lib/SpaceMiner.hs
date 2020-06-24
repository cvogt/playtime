module SpaceMiner where

-- import Music

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.IORef (atomicModifyIORef', newIORef)
import Game
import Graphics
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Extra
import My.IO
import My.Prelude
import SpaceMiner.Debug.Vty
import SpaceMiner.GL
import SpaceMiner.GLFW
import SpaceMiner.MutableState
import SpaceMiner.Types
import SpaceMiner.Util
import System.Exit (exitSuccess)

main :: IO ()
main = do
  --void $ forkIO $ forever $ playMusic
  visualizationMVar <- newEmptyMVar

  let logicalDimensions = Dimensions {width = 320, height = 240}
  let windowScale = ScaleInt 3

  withGLFW logicalDimensions windowScale "SpaceMiner" $ \window mutableState textures -> do
    gs <- initialGameState logicalDimensions <$> getSystemTime
    gameLoopDebugMVar <- newMVar (gs, [])
    renderLoopDebugMVar <- newMVar []
    totalLoopDebugMVar <- newMVar []

    void $ forkIO $ do
      flip unfoldM_ gs $ \oldGameState -> do
        gameLoopStartTime <- getSystemTime
        events <- fetchEvents mutableState
        let newGameState = foldl handleEvent oldGameState events
        visualization <- evaluate $ vizualizeGame textures newGameState
        gameLoopEndTime <- getSystemTime
        modifyMVar_ gameLoopDebugMVar $ \(_, times) -> pure (newGameState, timeDiffPico gameLoopStartTime gameLoopEndTime : times)
        threadDelay $ 10 * 1000
        putMVar visualizationMVar visualization
        pure $ if gsExitGame newGameState then Nothing else Just newGameState
      exitSuccess

    renderLoopStartIORef <- newIORef =<< getSystemTime
    void $ forkDebugTerminal gameLoopDebugMVar renderLoopDebugMVar totalLoopDebugMVar -- FIXME: cursor stays hidden after termination
    forever $ do
      GLFW.pollEvents
      visualization <- takeMVar visualizationMVar
      renderLoopStartTime <- getSystemTime
      previousRenderLoopStart <- atomicModifyIORef' renderLoopStartIORef $ \v -> (renderLoopStartTime, v)
      modifyMVar_ totalLoopDebugMVar $ pure . (timeDiffPico previousRenderLoopStart renderLoopStartTime :)
      renderGame window logicalDimensions visualization
      renderLoopEndTime <- getSystemTime
      modifyMVar_ renderLoopDebugMVar $ pure . (timeDiffPico renderLoopStartTime renderLoopEndTime :)

--putStrLn . show =<< getSystemTime

-- GL.deleteObjectNames [tex]
