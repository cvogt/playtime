{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Main where

import Codec.BMP (parseBMP)
import Codec.Picture (dynamicMap, encodeDynamicBitmap, imageHeight, imageWidth, readImage)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.FileEmbed
import Data.List (filter, reverse)
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
import My.IO
import My.Prelude
import System.Exit (exitSuccess)

windowWidth, windowHeight :: Float
windowWidth = 640
windowHeight = 480

main :: IO ()
main = do
  inputsMVar <- newMVar emptyCapturedInput
  glossState <- initState
  withWindow (float2Int windowWidth) (float2Int windowHeight) "Game-Demo" inputsMVar $ \win -> do
    --setCursorInputMode win CursorInputMode'Hidden
    loop glossState win initialGameState inputsMVar
    exitSuccess
  where
    loop glossState window oldGameState inputsMVar = do
      threadDelay 50000
      pollEvents
      capturedInputs <- modifyMVar inputsMVar $ \cs -> pure (emptyCapturedInput, cs)
      (x, y) <- GLFW.getCursorPos window
      let newGameState = handleEvents (x, y) oldGameState capturedInputs

      picture <- vizualizeGame (x, y) newGameState
      withModelview (float2Int windowWidth, float2Int windowHeight)
        $ withClearBuffer black
        $ do
          renderPicture glossState 1.0 picture

      swapBuffers window
      k <- keyIsPressed window Key'Q
      unless k $ loop glossState window newGameState inputsMVar

    -- move with cursor -- translate (int2Float $ gridify x') (int2Float $ gridify y')

    grid =
      fixPolyPos $ Color white $ Pictures $
        (([1 .. (windowWidth / gridsize)]) <&> \((* gridsize) -> x) -> Line [(x, 0), (x, windowHeight)])
          <> (([1 .. (windowHeight / gridsize)]) <&> \((* gridsize) -> y) -> Line [(0, y), (windowWidth, y)])
    fixPolyPos = translate (- (windowWidth / 2)) (- (windowHeight / 2))
    gridsize :: Float
    gridsize = 20
    gridify :: Int -> Int
    gridify = (* (float2Int gridsize)) . round . (/ gridsize) . int2Float

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed = True
isPress KeyState'Repeating = True
isPress _ = False
