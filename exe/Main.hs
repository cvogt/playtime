{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Main where

import Codec.BMP (parseBMP)
import Codec.Picture (dynamicMap, encodeDynamicBitmap, imageHeight, imageWidth, readImage)
import Control.Monad (unless, when)
import Control.Monad.Extra (unfoldM_)
import qualified Data.ByteString as BS
import Data.FileEmbed
import GHC.Float
import GHC.Real ((/), round)
import GLFWHelpers
import Game
import Graphics
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Rendering
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Music
import My.IO
import My.Prelude
import System.Exit (exitSuccess)

windowWidth, windowHeight :: Float
windowWidth = 640
windowHeight = 480

main :: IO ()
main = do
  --void $ forkIO $ forever $ playMusic
  eventsMVar <- newMVar []
  glossState <- initState
  time <- getSystemTime
  withWindow (float2Int windowWidth) (float2Int windowHeight) "Game-Demo" $ \window -> do
    pos <- GLFW.getCursorPos window
    startCaptureEvents window eventsMVar
    --setCursorInputMode win CursorInputMode'Hidden
    setWindowCloseCallback window $ Just $ \_ -> exitSuccess
    flip unfoldM_ (initialGameState time $ CursorPos pos) $ \oldGameState -> do
      waitEventsTimeout (1 / 60 :: Double)

      events <- fetchEvents eventsMVar

      let newGameState = foldl handleEvent oldGameState events

      picture <- vizualizeGame newGameState

      withModelview (float2Int windowWidth, float2Int windowHeight)
        $ withClearBuffer black
        $ do
          renderPicture glossState 1.0 picture

      swapBuffers window

      pure $ if gsExitGame newGameState then Nothing else Just newGameState
