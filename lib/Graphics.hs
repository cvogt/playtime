module Graphics where

import Codec.BMP (parseBMP)
import Codec.Picture (dynamicMap, encodeDynamicBitmap, imageHeight, imageWidth, readImage)
import Data.FileEmbed
import GHC.Float
import GHC.Real ((/), round)
import GLFWHelpers
import Game
import Graphics.Gloss
import My.IO
import My.Prelude

vizualizeGame :: GameState -> IO Picture
vizualizeGame gameState = do
  let g2gp = \(CursorPos (x, y)) -> (int2Float $ xg2g x, int2Float $ yg2g y)
  let board = gsBoard gameState <&> g2gp
  pic <- pictureFromFile $ $(makeRelativeToProject "assets/main_character.png" >>= strToExp) -- "/Users/chris/Downloads/Screenshot 2020-06-18 at 22.22.57.png"
  -- let blueSquare = Color blue $ Polygon [(0, 0), (0, 50), (50, 50), (50, 0)]
  let CursorPos (x, y) = gsCursorPos gameState
  let CursorPos (x', y') = gsMainCharacterPosition gameState
  pure $
    Pictures
      [ translate 0 100 $ Scale 0.2 0.2 $ Color white $ Text $ show (xg2g x, yg2g y)
      --, Color blue $ Polygon [(0,0),(0,50),(50,50),(50,0)]
      ]
      <> (uncurry translate (g2gp $ gsMainCharacterPosition gameState) $ Scale 5 5 pic)
      <> (Pictures $ (uncurry translate <$> board) <&> ($ pic))
      <> (Scale 4 4 $ uncurry translate (g2gp $ CursorPos (0, 0)) pic)
      <> (Scale 4 4 $ uncurry translate (g2gp $ CursorPos (100, 100)) pic)
      <> (translate 0 25 $ Scale 0.2 0.2 $ Color white $ Text $ "keys: " <> (show $ gsKeysPressed gameState))
      <> (translate 0 50 $ Scale 0.2 0.2 $ Color white $ Text $ "char: " <> show (round x' :: Int, round y' :: Int))
      <> (translate 0 75 $ Scale 0.2 0.2 $ Color white $ Text $ "char g2g: " <> show (g2gp $ gsMainCharacterPosition gameState))
      <> (Scale 0.2 0.2 $ Color white $ Text $ "openGL: " <> show (round x :: Int, round y :: Int))

pictureFromFile :: FilePath -> IO Picture
pictureFromFile path = do
  dynImage <- either fail pure =<< readImage path
  bmpBytes <- either (fail . show) pure $ encodeDynamicBitmap dynImage
  bmp <- either (fail . show) pure $ parseBMP bmpBytes
  pure $ translate ((int2Float $ dynamicMap imageWidth dynImage) / 2) ((int2Float $ dynamicMap imageHeight dynImage) / 2) $ bitmapOfBMP bmp

xg2g :: Double -> Int
xg2g x = (round x - 320)

yg2g :: Double -> Int
yg2g y = (round y - 240) * (-1)

-- move with cursor -- translate (int2Float $ gridify x') (int2Float $ gridify y')

-- grid =
--   fixPolyPos $ Color white $ Pictures $
--     (([1 .. (windowWidth / gridsize)]) <&> \((* gridsize) -> x) -> Line [(x, 0), (x, windowHeight)])
--       <> (([1 .. (windowHeight / gridsize)]) <&> \((* gridsize) -> y) -> Line [(0, y), (windowWidth, y)])
-- fixPolyPos = translate (- (windowWidth / 2)) (- (windowHeight / 2))
-- gridsize :: Float
-- gridsize = 20
-- gridify :: Int -> Int
-- gridify = (* (float2Int gridsize)) . round . (/ gridsize) . int2Float
