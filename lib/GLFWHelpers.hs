module GLFWHelpers where

import Codec.BMP
import Control.Monad (forM_, mapM_)
import qualified Data.ByteString.Unsafe as BSU
import Data.IORef (IORef, readIORef, writeIORef)
import Data.List (unwords)
import Data.List (reverse)
import Data.List ((++), map, zip)
import Data.Ord (max)
import Data.Word (Word8)
import Foreign (withForeignPtr)
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree, mallocBytes, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import GHC.Real ((/), fromIntegral, round)
import Graphics.Rendering.OpenGL (($=), get)
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU.Errors as GLU
import "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLUT as GLUT
import My.IO
import My.Prelude
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, makeStableName)
import Unsafe.Coerce (unsafeCoerce)
import Prelude (Read, String, error, return, unlines)

newtype CursorPos = CursorPos {unCursorPos :: (Int, Int)} deriving (Eq, Ord, Show)

data MouseEvent = MouseEvent
  { meButton :: MouseButton,
    meButtonState :: MouseButtonState,
    meModifierKeys :: ModifierKeys,
    meCursorPosition :: CursorPos
  }
  deriving (Show)

data KeyEvent = KeyEvent
  { keKey :: Key,
    keKeyState :: KeyState,
    keModifierKeys :: ModifierKeys
  }
  deriving (Show)

data CursorPosEvent = CursorPosEvent CursorPos
  deriving (Show)

data InputEvent
  = GameLoopEvent SystemTime
  | MouseEvent' MouseEvent
  | KeyEvent' KeyEvent
  | CursorPosEvent' CursorPosEvent
  | WindowCloseEvent
  deriving (Show)

startCaptureEvents :: Window -> MVar [InputEvent] -> IO ()
startCaptureEvents window mvar = do
  setMouseButtonCallback window $ Just $ \_ button state modifiers -> do
    (x, y) <- GLFW.getCursorPos window
    modifyMVar_ mvar $ pure . ((MouseEvent' $ MouseEvent button state modifiers $ CursorPos (round x, round y)) :)
  setKeyCallback window $ Just $ \_ key _scancode keyState modifiers ->
    modifyMVar_ mvar $ pure . ((KeyEvent' $ KeyEvent key keyState modifiers) :)
  setCursorPosCallback window $ Just $ \_ x y ->
    modifyMVar_ mvar $ pure . ((CursorPosEvent' $ CursorPosEvent $ CursorPos (round x, round y)) :)
  setWindowCloseCallback window $ Just $ \_ ->
    modifyMVar_ mvar $ pure . (WindowCloseEvent :)

fetchEvents :: MVar [InputEvent] -> IO [InputEvent]
fetchEvents eventsMVar = do
  pollEvents
  capturedInputs <- modifyMVar eventsMVar $ \cs -> pure ([], cs)
  time <- getSystemTime
  pure $ reverse $ GameLoopEvent time : capturedInputs

initGUI :: Window -> MVar [InputEvent] -> IO CursorPos
initGUI window eventsMVar = do
  (x, y) <- GLFW.getCursorPos window
  startCaptureEvents window eventsMVar
  --setCursorInputMode win CursorInputMode'Hidden
  pure $ CursorPos $ (round x, round y)

withWindow :: Int -> Int -> [Char] -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  Just _mon <- getPrimaryMonitor
  let fullscreen = Nothing -- (Just mon)
  when r $ do
    m <- GLFW.createWindow width height title fullscreen Nothing
    case m of
      (Just win) -> do
        GLFW.makeContextCurrent m
        GLFW.setErrorCallback $ Just simpleErrorCallback
        f win
        GLFW.destroyWindow win
      Nothing -> pure ()
    GLFW.terminate
  where
    simpleErrorCallback e s =
      putStrLn $ unwords [show e, show s]

renderGame :: Window -> IORef [Texture] -> Picture -> IO ()
renderGame window glossState picture = do
  (w, h) <- getWindowSize window
  withModelview (w, h) $ do
    renderPicture glossState 1.0 picture
  swapBuffers window

-- copied from Gloss for now:

-- | Set up the OpenGL rendering context for orthographic projection and run an
--   action to draw the model.
withModelview ::
  -- | Width and height of window.
  (Int, Int) ->
  -- | Action to perform.
  IO () ->
  IO ()
withModelview (sizeX, sizeY) action =
  do
    GL.matrixMode $= GL.Projection
    GL.preservingMatrix $
      do
        -- setup the co-ordinate system
        GL.loadIdentity
        let (sx, sy) = (fromIntegral sizeX / 2, fromIntegral sizeY / 2)
        GL.ortho (- sx) sx (- sy) sy 0 (-100)

        -- draw the world
        GL.matrixMode $= GL.Modelview 0

        -- initialization (done every time in this case)
        -- we don't need the depth buffer for 2d.
        GL.depthFunc GL.$= Just GL.Always

        GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1.0

        -- on every loop
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)

        GL.lineSmooth $= GL.Disabled
        GL.blend $= GL.Enabled
        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha) -- GL.blendFunc $= (GL.One, GL.Zero)
        action

        GL.matrixMode $= GL.Projection

    GL.matrixMode $= GL.Modelview 0

type Color = GL.Color4 Float

--------------------------------------
data Picture
  = Bitmap BitmapData
  | BitmapSection Rectangle BitmapData
  | Translate Float Float Picture
  | -- | Some text to draw with a vector font.
    Text String
  | Pictures [Picture]
  | Color Color Picture
  | -- | A picture scaled by the given x and y factors.
    Scale Float Float Picture
  deriving (Show, Eq)

-- | Abstract 32-bit RGBA bitmap data.
data BitmapData = BitmapData
  { bitmapDataLength :: Int, -- length (in bytes)
    bitmapFormat :: BitmapFormat,
    -- | width, height in pixels
    bitmapSize :: (Int, Int),
    bitmapCacheMe :: Bool,
    bitmapPointer :: (ForeignPtr Word8)
  }
  deriving (Show, Eq)

-- | Represents a rectangular section in a bitmap
data Rectangle = Rectangle
  { -- | x- and y-pos in the bitmap in pixels
    rectPos :: (Int, Int),
    -- | width/height of the area in pixelsi
    rectSize :: (Int, Int)
  }
  deriving (Show, Read, Eq, Ord)

data RowOrder
  = TopToBottom
  | BottomToTop
  deriving (Eq, Show, Ord)

-- | Pixel formats describe the order of the color channels in memory.
data PixelFormat
  = PxRGBA
  | PxABGR
  deriving (Eq, Show, Ord)

-- | Description of how the bitmap is layed out in memory.
--
--   * Prior version of Gloss assumed `BitmapFormat BottomToTop PxAGBR`
data BitmapFormat = BitmapFormat
  { rowOrder :: RowOrder,
    pixelFormat :: PixelFormat
  }
  deriving (Eq, Show)

-- | Render a picture into the current OpenGL context.
--
--   Assumes that the OpenGL matrix mode is set to @Modelview@
renderPicture ::
  -- | Current rendering state.
  IORef [Texture] ->
  -- | View port scale, which controls the level of detail.
  --   Use 1.0 to start with.
  Float ->
  -- | Picture to render.
  Picture ->
  IO ()
renderPicture state circScale picture =
  do
    -- Draw the picture
    checkErrors "before drawPicture."
    drawPicture state circScale picture
    checkErrors "after drawPicture."

drawPicture :: IORef [Texture] -> Float -> Picture -> IO ()
drawPicture state circScale picture =
  {-# SCC "drawComponent" #-}
  case picture of
    Pictures ps ->
      mapM_ (drawPicture state circScale) ps
    -- stroke text
    --      text looks weird when we've got blend on,
    --      so disable it during the renderString call.
    Text str ->
      do
        GL.blend $= GL.Disabled
        GL.preservingMatrix $ GLUT.renderString GLUT.Roman str
        GL.blend $= GL.Enabled
    Translate tx ty p ->
      GL.preservingMatrix $
        do
          GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
          drawPicture state circScale p
    Bitmap imgData ->
      let (width, height) = bitmapSize imgData
       in drawPicture state circScale $
            BitmapSection (rectAtOrigin width height) imgData
    -- Scale --------------------------------
    Scale sx sy p ->
      GL.preservingMatrix $
        do
          GL.scale (gf sx) (gf sy) 1
          let mscale = max sx sy
          drawPicture state (circScale * mscale) p
    Color col p -> do
      oldColor <- get GL.currentColor
      GL.currentColor $= col
      drawPicture state circScale p
      GL.currentColor $= oldColor
    BitmapSection
      Rectangle
        { rectPos = imgSectionPos,
          rectSize = imgSectionSize
        }
      imgData@BitmapData
        { bitmapSize = (width, height),
          bitmapCacheMe = cacheMe
        } ->
        do
          let rowInfo =
                -- calculate texture coordinates
                -- remark:
                --   On some hardware, using exact "integer" coordinates causes texture coords
                --   with a component == 0  flip to -1. This appears as the texture flickering
                --   on the left and sometimes show one additional row of pixels outside the
                --   given rectangle
                --   To prevent this we add an "epsilon-border".
                --   This has been testet to fix the problem.
                let defTexCoords =
                      map (\(x, y) -> (x / fromIntegral width, y / fromIntegral height)) $
                        [ vecMap (+ eps) (+ eps) $ toFloatVec imgSectionPos,
                          vecMap (subtract eps) (+ eps) $ toFloatVec $
                            ( fst imgSectionPos + fst imgSectionSize,
                              snd imgSectionPos
                            ),
                          vecMap (subtract eps) (subtract eps) $ toFloatVec $
                            ( fst imgSectionPos + fst imgSectionSize,
                              snd imgSectionPos + snd imgSectionSize
                            ),
                          vecMap (+ eps) (subtract eps) $ toFloatVec $
                            ( fst imgSectionPos,
                              snd imgSectionPos + snd imgSectionSize
                            )
                        ] ::
                        [(Float, Float)]
                    toFloatVec = vecMap fromIntegral fromIntegral
                    vecMap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
                    vecMap f g (x, y) = (f x, g y)
                    eps = 0.001 :: Float
                 in case rowOrder (bitmapFormat imgData) of
                      BottomToTop -> defTexCoords
                      TopToBottom -> reverse defTexCoords

          -- Load the image data into a texture,
          -- or grab it from the cache if we've already done that before.
          tex <- loadTexture state imgData cacheMe

          -- Set up wrap and filtering mode
          GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
          GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
          GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)

          -- Enable texturing
          GL.texture GL.Texture2D $= GL.Enabled
          GL.textureFunction $= GL.Combine

          -- Set current texture
          GL.textureBinding GL.Texture2D $= Just (texObject tex)

          -- Set to opaque
          oldColor <- get GL.currentColor
          GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0

          -- Draw textured polygon
          GL.renderPrimitive GL.Polygon
            $ forM_
              ( bitmapPath
                  (fromIntegral $ fst imgSectionSize)
                  (fromIntegral $ snd imgSectionSize)
                  `zip` rowInfo
              )
            $ \((polygonCoordX, polygonCoordY), (textureCoordX, textureCoordY)) ->
              do
                GL.texCoord $ GL.TexCoord2 (gf textureCoordX) (gf textureCoordY)
                GL.vertex $ GL.Vertex2 (gf polygonCoordX) (gf polygonCoordY)

          -- Restore color
          GL.currentColor $= oldColor

          -- Disable texturing
          GL.texture GL.Texture2D $= GL.Disabled

          -- Free uncachable texture objects.
          freeTexture tex

gf :: Float -> GL.GLfloat
gf x = unsafeCoerce x

freeTexture :: Texture -> IO ()
freeTexture tex
  | texCacheMe tex = return ()
  | otherwise = GL.deleteObjectNames [texObject tex]

data Texture = Texture
  { -- | Stable name derived from the `BitmapData` that the user gives us.
    texName :: StableName BitmapData,
    -- | Width of the image, in pixels.
    texWidth :: Int,
    -- | Height of the image, in pixels.
    texHeight :: Int,
    -- | Pointer to the Raw texture data.
    texData :: ForeignPtr Word8,
    -- | The OpenGL texture object.
    texObject :: GL.TextureObject,
    -- | Whether we want to leave this in OpenGL texture memory between frames.
    texCacheMe :: Bool
  }

bitmapPath :: Float -> Float -> [(Float, Float)]
bitmapPath width height =
  [(- width', - height'), (width', - height'), (width', height'), (- width', height')]
  where
    width' = width / 2
    height' = height / 2

loadTexture ::
  -- | Existing texture cache.
  IORef [Texture] ->
  -- | Texture data.
  BitmapData ->
  -- | Force cache for newly loaded textures.
  Bool ->
  IO Texture
loadTexture refTextures imgData@BitmapData {bitmapSize = (width, height)} cacheMe =
  do
    textures <- readIORef refTextures

    -- Try and find this same texture in the cache.
    name <- makeStableName imgData
    let mTexCached =
          find
            ( \tex ->
                texName tex == name
                  && texWidth tex == width
                  && texHeight tex == height
            )
            textures

    case mTexCached of
      Just tex ->
        return tex
      Nothing ->
        do
          tex <- installTexture imgData
          when cacheMe $
            writeIORef refTextures (tex : textures)
          return tex

installTexture :: BitmapData -> IO Texture
installTexture bitmapData@(BitmapData _ fmt (width, height) cacheMe fptr) =
  do
    let glFormat =
          case pixelFormat fmt of
            PxABGR -> GL.ABGR
            PxRGBA -> GL.RGBA

    -- Allocate texture handle for texture
    [tex] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D $= Just tex

    -- Sets the texture in imgData as the current texture
    -- This copies the data from the pointer into OpenGL texture memory,
    -- so it's ok if the foreignptr gets garbage collected after this.
    withForeignPtr fptr $
      \ptr ->
        GL.texImage2D
          GL.Texture2D
          GL.NoProxy
          0
          GL.RGBA8
          ( GL.TextureSize2D
              (gsizei width)
              (gsizei height)
          )
          0
          (GL.PixelData glFormat GL.UnsignedByte ptr)

    -- Make a stable name that we can use to identify this data again.
    -- If the user gives us the same texture data at the same size then we
    -- can avoid loading it into texture memory again.
    name <- makeStableName bitmapData

    return
      Texture
        { texName = name,
          texWidth = width,
          texHeight = height,
          texData = fptr,
          texObject = tex,
          texCacheMe = cacheMe
        }

-- | Used for similar reasons to above
gsizei :: Int -> GL.GLsizei
gsizei x = unsafeCoerce x

-- | Construct a rectangle of the given width and height,
--   with the lower left corner at the origin.
rectAtOrigin :: Int -> Int -> Rectangle
rectAtOrigin w h = Rectangle (0, 0) (w, h)

checkErrors :: String -> IO ()
checkErrors place =
  do
    errors <- get $ GLU.errors
    when (not $ null errors) $
      mapM_ (handleError place) errors

handleError :: String -> GLU.Error -> IO ()
handleError place err =
  case err of
    GLU.Error GLU.StackOverflow _ ->
      error $
        unlines
          [ "Gloss / OpenGL Stack Overflow " ++ show place,
            "  This program uses the Gloss vector graphics library, which tried to",
            "  draw a picture using more nested transforms (Translate/Rotate/Scale)",
            "  than your OpenGL implementation supports. The OpenGL spec requires",
            "  all implementations to have a transform stack depth of at least 32,",
            "  and Gloss tries not to push the stack when it doesn't have to, but",
            "  that still wasn't enough.",
            "",
            "  You should complain to your harware vendor that they don't provide",
            "  a better way to handle this situation at the OpenGL API level.",
            "",
            "  To make this program work you'll need to reduce the number of nested",
            "  transforms used when defining the Picture given to Gloss. Sorry."
          ]
    -- Issue #32: Spurious "Invalid Operation" errors under Windows 7 64-bit.
    --   When using GLUT under Windows 7 it complains about InvalidOperation,
    --   but doesn't provide any other details. All the examples look ok, so
    --   we're just ignoring the error for now.
    GLU.Error GLU.InvalidOperation _ ->
      return ()
    _ ->
      error $
        unlines
          [ "Gloss / OpenGL Internal Error " ++ show place,
            "  Please report this on haskell-gloss@googlegroups.com.",
            show err
          ]

-- | O(size). Copy a `BMP` file into a bitmap.
bitmapOfBMP :: BMP -> Picture
bitmapOfBMP bmp =
  Bitmap $ bitmapDataOfBMP bmp

-- | O(size). Copy a `BMP` file into a bitmap.
bitmapDataOfBMP :: BMP -> BitmapData
bitmapDataOfBMP bmp =
  unsafePerformIO $
    do
      let (width, height) = bmpDimensions bmp
      let bs = unpackBMPToRGBA32 bmp
      let len = width * height * 4

      ptr <- mallocBytes len
      fptr <- newForeignPtr finalizerFree ptr

      BSU.unsafeUseAsCString bs $
        \cstr -> copyBytes ptr (castPtr cstr) len

      return $ BitmapData len (BitmapFormat BottomToTop PxRGBA) (width, height) True fptr
