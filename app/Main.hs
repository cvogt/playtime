{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

{-# LANGUAGE PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Control.Monad(fail)
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import System.Exit ( exitSuccess )
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless)
import Codec.Picture (decodePng, encodeDynamicBitmap, readImage, dynamicMap, imageWidth, imageHeight)
import Codec.BMP (parseBMP)
import GHC.Float
import System.Posix.Process
import Protolude hiding (State)
import qualified Data.ByteString as BS
import           Data.FileEmbed

import Euterpea hiding (Text, forever)

windowWidth, windowHeight :: Float
windowWidth  = 640
windowHeight = 480

type Clicks = [(Double, Double)]

main :: IO ()
main = do
  --forkIO $ executeFile "/usr/local/bin/fluidsynth" False [$(makeRelativeToProject "Steinway+Grand+Piano+ER3A.sf2" >>= strToExp)] Nothing

  clicksMVar :: MVar Clicks <- newMVar []
  mouseDownMVar :: MVar Bool <- newMVar False
  glossState <- initState
  withWindow (float2Int windowWidth) (float2Int windowHeight) "Game-Demo" clicksMVar mouseDownMVar $ \win -> do
    -- let p1 = [ c 5 qn, e 5 qn, d 5 en, c 5 en, e 5 qn, c 5 qn, b 4 en, e 5 en, a 4 hn ]
    -- let p2 = [ e 5 qn, e 5 qn, g 5 qn, g 5 en, a 5 en, f 5 qn, a 5 qn, e 5 hn ]
    -- let p3 = [ c 6 qn, b 5 en, a 5 en, b 5 qn, e 5 qn, a 5 qn, e 5 en, d 5 en, e 5 qn, b 4 qn ]
    -- let p = Euterpea.play . Euterpea.line
    -- forkIO $ forever $ p $ [chord [ c 4 en, e 4 en, g 4 en ], chord [ c 4 en, e 4 en, g 4 en ], chord [ c 4 en, e 4 en, g 4 en ], chord [ a 3 hn, c 4 hn, e 4 hn ]]  -- p1 <> p2 <> p3


    loop glossState win [] clicksMVar mouseDownMVar
    exitSuccess
  where
    loop glossState window gameState clicksMVar mouseDownMVar =  do
      threadDelay 50000
      pollEvents
      clicks <- modifyMVar clicksMVar $ \cs -> pure ([], cs)
      mouseDown <- readMVar mouseDownMVar

      --setCursorInputMode window CursorInputMode'Hidden
      (x,y) <- GLFW.getCursorPos window
      let newGameState = gameState <> clicks <> (if mouseDown then [(x,y)] else [])
      let newGameState' = newGameState <&> \(x,y) -> (int2Float $ xg2g x,int2Float $ yg2g y)
      pic <- pictureFromFile $ $(makeRelativeToProject "assets/main_character.png" >>= strToExp)
      let blueSquare = Color blue $ Polygon [(0,0),(0,50),(50,50),(50,0)]
      let
        picture = Pictures
          [ translate 0 100 $ Scale 0.2 0.2 $ Color white $ Text $ show (xg2g x,yg2g y)
          --, Color blue $ Polygon [(0,0),(0,50),(50,50),(50,0)]
          ] <> (Pictures $ (uncurry translate <$> newGameState') <&> ($ pic))
      withModelview (float2Int windowWidth, float2Int windowHeight)
        $ withClearBuffer black
        $ do
            renderPicture glossState 1.0 picture
            renderPicture glossState 1.0 $ Scale 0.2 0.2 $ Color white $ Text $ "openGL: " <> show (round x :: Int,round y :: Int)

      swapBuffers window
      k <- keyIsPressed window Key'Q
      unless k $ loop glossState window newGameState clicksMVar mouseDownMVar

    -- move with cursor -- translate (int2Float $ gridify x') (int2Float $ gridify y') 

    grid = fixPolyPos $ Color white $ Pictures $
      (([1 .. (windowWidth / gridsize)]) <&> \((*gridsize) -> x)  -> Line [(x,0),(x,windowHeight)])
      <>
      (([1 .. (windowHeight / gridsize)]) <&> \((*gridsize) -> y)  -> Line [(0,y),(windowWidth,y)])

    fixPolyPos = translate (-(windowWidth/2)) (-(windowHeight/2))
    gridsize :: Float
    gridsize = 20
    gridify :: Int -> Int
    gridify = (*(float2Int gridsize)) . round . (/gridsize) . int2Float

xg2g :: Double -> Int
xg2g x = (round x - 320)
yg2g :: Double -> Int
yg2g y = (round y - 240) * (-1)

pictureFromFile :: FilePath -> IO Picture
pictureFromFile path = do
  dynImage <- either fail pure =<< readImage path
  bmpBytes <- either (fail . show) pure $ encodeDynamicBitmap dynImage
  bmp <- either (fail . show) pure $ parseBMP bmpBytes
  pure $ translate ((int2Float $ dynamicMap imageWidth dynImage)/2) ((int2Float $ dynamicMap imageHeight dynImage)/2) $ bitmapOfBMP bmp

withWindow :: Int -> Int -> [Char] -> MVar Clicks -> MVar Bool -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title clicksMVar mouseDownMVar f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    Just mon <- getPrimaryMonitor
    let fullscreen = Nothing -- (Just mon)
    when r $ do
        m <- GLFW.createWindow width height title fullscreen Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              setMouseButtonCallback win $ Just $ \_ _ mbs _ -> case mbs of
                MouseButtonState'Released -> do
                  (x,y) <- GLFW.getCursorPos win
                  modifyMVar_ clicksMVar $ pure . ((x,y) :)
                  modifyMVar_ mouseDownMVar $ \_ -> pure False
                _ -> do
                  modifyMVar_ mouseDownMVar $ \_ -> pure True
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                  = False
