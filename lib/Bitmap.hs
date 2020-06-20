module Bitmap where

import Codec.BMP (BMP, bmpDimensions, parseBMP, unpackBMPToRGBA32)
import Codec.Picture (encodeDynamicBitmap, readImage)
import qualified Data.ByteString.Unsafe as BSU
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree, mallocBytes, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import GLFWHelpers
import My.IO
import My.Prelude
import System.IO.Unsafe (unsafePerformIO)

-- | O(size). Copy a `BMP` file into a bitmap.
bitmapOfBMP :: BMP -> BitmapData
bitmapOfBMP bmp =
  unsafePerformIO $ do
    let (width, height) = bmpDimensions bmp
    let bs = unpackBMPToRGBA32 bmp
    let len = width * height * 4
    ptr <- mallocBytes len
    fptr <- newForeignPtr finalizerFree ptr
    BSU.unsafeUseAsCString bs $ \cstr -> copyBytes ptr (castPtr cstr) len
    pure $ BitmapData (width, height) fptr

pictureFromFile :: FilePath -> IO Texture
pictureFromFile path = do
  dynImage <- either fail pure =<< readImage path
  bmpBytes <- either (fail . show) pure $ encodeDynamicBitmap dynImage
  bmp <- either (fail . show) pure $ parseBMP bmpBytes
  let imgData@(BitmapData xy _) = bitmapOfBMP bmp
  tex <- sendTextureToGL imgData
  pure $ Texture xy tex
