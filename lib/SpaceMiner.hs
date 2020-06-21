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
import System.Exit (exitSuccess)

main :: Int -> Int -> Int -> IO ()
main width height _fps = do
  --void $ forkIO $ forever $ playMusic
  eventsMVar <- newMVar []
  visualizationMVar <- newEmptyMVar

  withWindow width height "SpaceMiner" $ \window -> do
    initGUI window eventsMVar
    pic <- loadPic

    void $ forkIO $ do
      time <- getSystemTime
      flip unfoldM_ (initialGameState time) $ \oldGameState -> do
        events <- fetchEvents eventsMVar
        let newGameState = foldl handleEvent oldGameState events
        visualization <- vizualizeGame pic newGameState
        putMVar visualizationMVar visualization
        pure $ if gsExitGame newGameState then Nothing else Just newGameState
      exitSuccess

    void forkDebugTerminal -- FIXME: cursor stays hidden after termination
    forever $ do
      pollEvents
      visualization <- takeMVar visualizationMVar
      renderGame window visualization

--putStrLn . show =<< getSystemTime

-- GL.deleteObjectNames [tex]
