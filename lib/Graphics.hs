module Graphics where

import Codec.BMP (parseBMP)
import Codec.Picture (dynamicMap, encodeDynamicBitmap, imageHeight, imageWidth, readImage)
import GHC.Float
import GHC.Real ((/))
import Graphics.Gloss
import My.IO
import My.Prelude

pictureFromFile :: FilePath -> IO Picture
pictureFromFile path = do
  dynImage <- either fail pure =<< readImage path
  bmpBytes <- either (fail . show) pure $ encodeDynamicBitmap dynImage
  bmp <- either (fail . show) pure $ parseBMP bmpBytes
  pure $ translate ((int2Float $ dynamicMap imageWidth dynImage) / 2) ((int2Float $ dynamicMap imageHeight dynImage) / 2) $ bitmapOfBMP bmp
