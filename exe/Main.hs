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
import Data.List (unwords)
import GHC.Float
import GHC.Real ((/), round)
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

type Clicks = [(Double, Double)]

main :: IO ()
main = do
  clicksMVar :: MVar Clicks <- newMVar []
  mouseDownMVar :: MVar Bool <- newMVar False
  glossState <- initState
  withWindow (float2Int windowWidth) (float2Int windowHeight) "Game-Demo" clicksMVar mouseDownMVar $ \win -> do
    loop glossState win [] clicksMVar mouseDownMVar
    exitSuccess
  where
    loop glossState window gameState clicksMVar mouseDownMVar = do
      threadDelay 50000
      pollEvents
      clicks <- modifyMVar clicksMVar $ \cs -> pure ([], cs)
      mouseDown <- readMVar mouseDownMVar

      --setCursorInputMode window CursorInputMode'Hidden
      (x, y) <- GLFW.getCursorPos window
      let newGameState = gameState <> clicks <> (if mouseDown then [(x, y)] else [])
      let newGameState' = newGameState <&> \(x, y) -> (int2Float $ xg2g x, int2Float $ yg2g y)
      pic <- pictureFromFile $ $(makeRelativeToProject "assets/main_character.png" >>= strToExp)
      let blueSquare = Color blue $ Polygon [(0, 0), (0, 50), (50, 50), (50, 0)]
      let picture =
            Pictures
              [ translate 0 100 $ Scale 0.2 0.2 $ Color white $ Text $ show (xg2g x, yg2g y)
              --, Color blue $ Polygon [(0,0),(0,50),(50,50),(50,0)]
              ]
              <> (Pictures $ (uncurry translate <$> newGameState') <&> ($ pic))
      withModelview (float2Int windowWidth, float2Int windowHeight)
        $ withClearBuffer black
        $ do
          renderPicture glossState 1.0 picture
          renderPicture glossState 1.0 $ Scale 0.2 0.2 $ Color white $ Text $ "openGL: " <> show (round x :: Int, round y :: Int)

      swapBuffers window
      k <- keyIsPressed window Key'Q
      unless k $ loop glossState window newGameState clicksMVar mouseDownMVar

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

xg2g :: Double -> Int
xg2g x = (round x - 320)

yg2g :: Double -> Int
yg2g y = (round y - 240) * (-1)

pictureFromFile :: FilePath -> IO Picture
pictureFromFile path = do
  dynImage <- either fail pure =<< readImage path
  bmpBytes <- either (fail . show) pure $ encodeDynamicBitmap dynImage
  bmp <- either (fail . show) pure $ parseBMP bmpBytes
  pure $ translate ((int2Float $ dynamicMap imageWidth dynImage) / 2) ((int2Float $ dynamicMap imageHeight dynImage) / 2) $ bitmapOfBMP bmp

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
            (x, y) <- GLFW.getCursorPos win
            modifyMVar_ clicksMVar $ pure . ((x, y) :)
            modifyMVar_ mouseDownMVar $ \_ -> pure False
          _ -> do
            modifyMVar_ mouseDownMVar $ \_ -> pure True
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
      Nothing -> pure ()
    GLFW.terminate
  where
    simpleErrorCallback e s =
      putStrLn $ unwords [show e, show s]

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed = True
isPress KeyState'Repeating = True
isPress _ = False
