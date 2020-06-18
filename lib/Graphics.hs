module Graphics where

import Codec.BMP (parseBMP)
import Codec.Picture (dynamicMap, encodeDynamicBitmap, imageHeight, imageWidth, readImage)
import Data.FileEmbed
import GHC.Float
import GHC.Real ((/), round)
import Game
import Graphics.Gloss
import My.IO
import My.Prelude

vizualizeGame :: (Double, Double) -> GameState -> IO Picture
vizualizeGame (x, y) gameState = do
  let board = gsBoard gameState <&> \(x', y') -> (int2Float $ xg2g x', int2Float $ yg2g y')
  pic <- pictureFromFile $ $(makeRelativeToProject "assets/main_character.png" >>= strToExp)
  -- let blueSquare = Color blue $ Polygon [(0, 0), (0, 50), (50, 50), (50, 0)]
  pure $
    Pictures
      [ translate 0 100 $ Scale 0.2 0.2 $ Color white $ Text $ show (xg2g x, yg2g y)
      --, Color blue $ Polygon [(0,0),(0,50),(50,50),(50,0)]
      ]
      <> (Pictures $ (uncurry translate <$> board) <&> ($ pic))
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
