module Bitmap where

import Codec.BMP (BMP, bmpDimensions, parseBMP, unpackBMPToRGBA32)
import Codec.Picture (dynamicMap, encodeDynamicBitmap, imageHeight, imageWidth, readImage)
import qualified Data.ByteString.Unsafe as BSU
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree, mallocBytes, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import GHC.Float (int2Float)
import GHC.Real ((/))
import GLFWHelpers
import My.IO
import My.Prelude
import System.IO.Unsafe (unsafePerformIO)

-- | O(size). Copy a `BMP` file into a bitmap.
bitmapOfBMP :: BMP -> Picture
bitmapOfBMP bmp =
  Bitmap $ unsafePerformIO $ do
    let (width, height) = bmpDimensions bmp
    let bs = unpackBMPToRGBA32 bmp
    let len = width * height * 4
    ptr <- mallocBytes len
    fptr <- newForeignPtr finalizerFree ptr
    BSU.unsafeUseAsCString bs $ \cstr -> copyBytes ptr (castPtr cstr) len
    pure $ BitmapData len (width, height) fptr

pictureFromFile :: FilePath -> IO Picture
pictureFromFile path = do
  dynImage <- either fail pure =<< readImage path
  bmpBytes <- either (fail . show) pure $ encodeDynamicBitmap dynImage
  bmp <- either (fail . show) pure $ parseBMP bmpBytes
  pure $ Translate ((int2Float $ dynamicMap imageWidth dynImage) / 2) ((int2Float $ dynamicMap imageHeight dynImage) / 2) $ bitmapOfBMP bmp
