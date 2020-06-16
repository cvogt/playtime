{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

import Protolude hiding (rotate)

import Lib

main :: IO ()
main = do
    let width  = 640
        height = 480
    withWindow width height "Resurrection" $ \window -> do
      glossState <- initState
      renderFrame width height window glossState
      threadDelay $ 1000 * 1000 * 10

withWindow :: Int -> Int -> [Char] -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

renderFrame width height window glossState = do
   displayPicture (width, height) white glossState 1.0 $
     Pictures
       [ Color violet $ translate (-300) 100 $
             polygon [((-10), 10), ((-10), 70), (20, 20), (20, 30)]
       , Color red $ translate (-200) 100 $
             line [(-30, -30), (-40, 30), (30, 40), (50, -20)]
       , Color (makeColor 0 128 255 1) $ translate (-100) 100 $
             lineLoop [(-30, -30), (-40, 30), (30, 40), (50, -20)]
       , Color red $ translate 0 100 $
             circle 30
       , Color green $ translate 100 100 $
             thickCircle 30 10
       , Color yellow $ translate 200 100 $
             circleSolid 30
       , Color chartreuse $ translate (-200) (-100) $
             thickArc 0 180 30 30
       , Color (dark magenta) $ translate (-100) (-100) $
             arcSolid 0 90 30
       , Color (bright magenta) $ translate 0 (-100) $ scale 0.2 0.2 $
             text "Boo!"
       , Color (dim cyan) $ translate 100 (-100) $ rotate 30 $
             rectangleWire 20 50
       , Color (light cyan) $ translate 200 (-100) $ rotate 60 $
             rectangleSolid 20 50 ]
