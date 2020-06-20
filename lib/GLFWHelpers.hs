module GLFWHelpers where

import Control.Monad (forM_, mapM_)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.List (unwords)
import Data.List (reverse)
import Data.List (map, zip)
import Data.Ord (max)
import Data.Word (Word8)
import Foreign (withForeignPtr)
import GHC.Real ((/), fromIntegral, round)
import Graphics.Rendering.OpenGL (($=), get)
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU.Errors as GLU
import "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLUT as GLUT
import My.IO
import My.Prelude
import System.Mem.StableName (StableName, makeStableName)
import Unsafe.Coerce (unsafeCoerce)
import Prelude (String, error)

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

renderGame :: Window -> TextureCache -> Picture -> IO ()
renderGame window textureCache picture = do
  (width, height) <- getWindowSize window
  GL.matrixMode $= GL.Projection
  GL.preservingMatrix $ do
    -- setup the co-ordinate system
    GL.loadIdentity
    let (sx, sy) = (fromIntegral width / 2, fromIntegral height / 2)
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
    void $ error . show <$> get GLU.errors
    drawPicture textureCache 1.0 picture
    void $ error . show <$> get GLU.errors
    swapBuffers window

    GL.matrixMode $= GL.Projection

  GL.matrixMode $= GL.Modelview 0

type Color = GL.Color4 Float

--------------------------------------
data Picture
  = Bitmap BitmapData
  | Translate Float Float Picture
  | -- | Some text to draw with a vector font.
    Text Color String
  | Pictures [Picture]
  | -- | A picture scaled by the given x and y factors.
    Scale Float Float Picture
  deriving (Show, Eq)

-- | Abstract 32-bit RGBA bitmap data.
data BitmapData = BitmapData
  { bitmapSize :: (Int, Int),
    bitmapPointer :: (ForeignPtr Word8)
  }
  deriving (Show, Eq)

drawPicture :: TextureCache -> Float -> Picture -> IO ()
drawPicture state circScale picture =
  {-# SCC "drawComponent" #-}
  case picture of
    Pictures ps ->
      mapM_ (drawPicture state circScale) ps
    -- stroke text
    --      text looks weird when we've got blend on,
    --      so disable it during the renderString call.
    Text col str ->
      do
        oldColor <- get GL.currentColor
        GL.currentColor $= col
        GL.blend $= GL.Disabled
        GL.preservingMatrix $ GLUT.renderString GLUT.Roman str
        GL.blend $= GL.Enabled
        GL.currentColor $= oldColor
    Translate tx ty p ->
      GL.preservingMatrix $
        do
          GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
          drawPicture state circScale p
    Scale sx sy p ->
      GL.preservingMatrix $
        do
          GL.scale (gf sx) (gf sy) 1
          let mscale = max sx sy
          drawPicture state (circScale * mscale) p
    Bitmap imgData -> do
      let cacheMe = True
          (width, height) = bitmapSize imgData
          imgSectionPos = (0, 0)
          imgSectionSize = (width, height)
          rowInfo =
            -- calculate texture coordinates
            -- remark:
            --   On some hardware, using exact "integer" coordinates causes texture coords
            --   with a component == 0  flip to -1. This appears as the texture flickering
            --   on the left and sometimes show one additional row of pixels outside the
            --   given rectangle
            --   To prevent this we add an "epsilon-border".
            --   This has been testet to fix the problem.
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
            where
              toFloatVec = vecMap fromIntegral fromIntegral
              vecMap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
              vecMap f g (x, y) = (f x, g y)
              eps = 0.001 :: Float

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
      GL.textureBinding GL.Texture2D $= Just tex

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
      when (not cacheMe) $ GL.deleteObjectNames [tex]
  where
    gf = unsafeCoerce :: Float -> GL.GLfloat

data Texture = Texture
  { -- | Stable name derived from the `BitmapData` that the user gives us.
    texName :: StableName BitmapData,
    -- | Pointer to the Raw texture data.
    texData :: ForeignPtr Word8,
    -- | The OpenGL texture object.
    texObject :: GL.TextureObject
  }

bitmapPath :: Float -> Float -> [(Float, Float)]
bitmapPath width height =
  [(- width', - height'), (width', - height'), (width', height'), (- width', height')]
  where
    width' = width / 2
    height' = height / 2

type TextureCache = IORef [(StableName BitmapData, GLUT.TextureObject)]

loadTexture ::
  -- | Existing texture cache.
  TextureCache ->
  -- | Texture data.
  BitmapData ->
  -- | Force cache for newly loaded textures.
  Bool ->
  IO GLUT.TextureObject
loadTexture textureCache imgData cacheMe = do
  -- Try and find this same texture in the cache.
  name <- makeStableName imgData
  textures <- readIORef textureCache
  maybe (installTexture' name) (pure . snd) $ find ((name==).fst) textures
  where
    installTexture' name = do
      textures <- readIORef textureCache
      tex <- installTexture imgData
      when cacheMe $ writeIORef textureCache $ (name,tex) : textures
      pure tex

installTexture :: BitmapData -> IO GLUT.TextureObject
installTexture (BitmapData (width, height) fptr) = do
  let txSize = GL.TextureSize2D (unsafeCoerce width) (unsafeCoerce height)
  [texture] <- GL.genObjectNames 1
  GL.textureBinding GL.Texture2D $= Just texture
  withForeignPtr fptr $ GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 txSize 0 . GL.PixelData GL.RGBA GL.UnsignedByte
  pure texture
