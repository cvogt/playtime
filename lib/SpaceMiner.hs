module SpaceMiner where

-- import Music

import GHC.Float (int2Double)
import GHC.Real ((/))
import GLFWHelpers (fetchEvents, initGUI, initState, renderGame, withWindow)
import Game (gsExitGame, handleEvent, initialGameState)
import Graphics (loadPic, vizualizeGame)
import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Extra
import My.IO
import My.Prelude

main :: Int -> Int -> Int -> IO ()
main width height fps = do
  --void $ forkIO $ forever $ playMusic
  eventsMVar <- newMVar []
  glossState <- initState
  time <- getSystemTime

  withWindow width height "Game-Demo" $ \window -> do
    pos <- initGUI window eventsMVar
    pic <- loadPic

    flip unfoldM_ (initialGameState time pos) $ \oldGameState -> do
      when False $ waitEventsTimeout (1 / int2Double fps :: Double) -- FIXME: subtract time taken by loop run
      --putStrLn "--------------------------------"
      --putStrLn . show =<< getSystemTime
      events <- fetchEvents eventsMVar
      --putStrLn . show =<< getSystemTime
      let newGameState = foldl handleEvent oldGameState events
      --putStrLn . show =<< getSystemTime
      --when (length events > 1) $

      visualization <- vizualizeGame pic newGameState
      --putStrLn . show =<< getSystemTime

      renderGame window glossState visualization
      --putStrLn . show =<< getSystemTime

      pure $ if gsExitGame newGameState then Nothing else Just newGameState
