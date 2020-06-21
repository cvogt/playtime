module SpaceMiner where

-- import Music

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import GLFWHelpers (fetchEvents, initGUI, renderGame, withWindow)
import Game (gsExitGame, handleEvent, initialGameState)
import Graphics (loadPic, vizualizeGame)
import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Extra
import My.IO
import My.Prelude
import SpaceMiner.Debug.Vty (forkDebugTerminal)
import SpaceMiner.Util (timeDiffPico)
import System.Exit (exitSuccess)

--import Data.IORef (newIORef)

main :: Int -> Int -> Int -> IO ()
main width height _fps = do
  --void $ forkIO $ forever $ playMusic
  eventsMVar <- newMVar []
  visualizationMVar <- newEmptyMVar

  withWindow width height "SpaceMiner" $ \window -> do
    initGUI window width height eventsMVar
    pic <- loadPic

    gs <- initialGameState <$> getSystemTime
    gameLoopDebugMVar <- newMVar (gs, [])
    renderLoopDebugMVar <- newMVar []

    void $ forkIO $ do
      flip unfoldM_ gs $ \oldGameState -> do
        gameLoopStartTime <- getSystemTime
        events <- fetchEvents eventsMVar
        let newGameState = foldl handleEvent oldGameState events
        visualization <- evaluate $ vizualizeGame pic newGameState
        gameLoopEndTime <- getSystemTime
        modifyMVar_ gameLoopDebugMVar $ \(_, times) -> pure (newGameState, timeDiffPico gameLoopStartTime gameLoopEndTime : times)
        putMVar visualizationMVar visualization
        pure $ if gsExitGame newGameState then Nothing else Just newGameState
      exitSuccess

    void $ forkDebugTerminal gameLoopDebugMVar renderLoopDebugMVar -- FIXME: cursor stays hidden after termination
    forever $ do
      pollEvents
      visualization <- takeMVar visualizationMVar
      renderLoopStartTime <- getSystemTime
      renderGame window visualization
      renderLoopEndTime <- getSystemTime
      modifyMVar_ renderLoopDebugMVar $ pure . (timeDiffPico renderLoopStartTime renderLoopEndTime :)

--putStrLn . show =<< getSystemTime

-- GL.deleteObjectNames [tex]
