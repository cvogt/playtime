module SpaceMiner where

-- import Music

import GHC.Float (int2Double)
import GHC.Real ((/))
import GLFWHelpers
import Game
import Graphics
import qualified Graphics.Gloss.Rendering as GlossRendering
import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Extra
import My.IO
import My.Prelude

main :: Int -> Int -> Int -> IO ()
main width height fps = do
  --void $ forkIO $ forever $ playMusic
  eventsMVar <- newMVar []
  glossState <- GlossRendering.initState
  time <- getSystemTime

  withWindow width height "Game-Demo" $ \window -> do
    pos <- initGUI window eventsMVar

    flip unfoldM_ (initialGameState time pos) $ \oldGameState -> do
      waitEventsTimeout (1 / int2Double fps :: Double) -- FIXME: subtract time taken by loop run
      events <- fetchEvents eventsMVar
      let newGameState = foldl handleEvent oldGameState events
      renderGame window glossState =<< vizualizeGame newGameState
      pure $ if gsExitGame newGameState then Nothing else Just newGameState
