module GLFWHelpers where

import Control.Monad (mapM_)
import Data.List (reverse, unwords)
import Data.Word (Word8)
import GHC.Float (double2Float, int2Double)
import GHC.Real ((/), fromIntegral)
import Graphics.Rendering.OpenGL (($=), get)
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU.Errors as GLU
import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.IO
import My.Prelude
import Unsafe.Coerce (unsafeCoerce)
import Codec.Picture (readPng, DynamicImage(ImageRGBA8), Image(Image))
import GHC.Err (error)
import Data.Vector.Storable (unsafeWith)

newtype CursorPos = CursorPos {unCursorPos :: (Double, Double)} deriving (Eq, Ord, Show)

data MouseEvent = MouseEvent
  { meButton :: MouseButton,
    meButtonState :: MouseButtonState,
    meModifierKeys :: ModifierKeys
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

startCaptureEvents :: Window -> Int -> Int -> MVar [InputEvent] -> IO ()
startCaptureEvents window (int2Double -> logicWidth) (int2Double -> logicHeight) mvar = do
  setMouseButtonCallback window $ Just $ \_ button state modifiers -> do
    modifyMVar_ mvar $ pure . ((MouseEvent' $ MouseEvent button state modifiers) :)
  setKeyCallback window $ Just $ \_ key _scancode keyState modifiers ->
    modifyMVar_ mvar $ pure . ((KeyEvent' $ KeyEvent key keyState modifiers) :)
  setCursorPosCallback window $ Just $ \window' x y -> do
    -- this ratio calculation leads to proper relative scaling on window resize
    -- FIXME: we still get distortion if aspect ration of resized window is different
    --        we should be able to fix that by adding black borders as needed
    (int2Double -> actualWidth, int2Double -> actualHeight) <- getWindowSize window'
    let wratio = actualWidth / logicWidth
        hratio = actualHeight / logicHeight
    modifyMVar_ mvar $ pure . ((CursorPosEvent' $ CursorPosEvent $ CursorPos (x / wratio, y / hratio)) :)
  setWindowCloseCallback window $ Just $ \_ ->
    modifyMVar_ mvar $ pure . (WindowCloseEvent :)

fetchEvents :: MVar [InputEvent] -> IO [InputEvent]
fetchEvents eventsMVar = do
  capturedInputs <- modifyMVar eventsMVar $ \cs -> pure ([], cs)
  time <- getSystemTime
  pure $ reverse $ GameLoopEvent time : capturedInputs

initGUI :: Window -> Int -> Int -> MVar [InputEvent] -> IO () -- CursorPos
initGUI window width height eventsMVar = do
  --(x, y) <- GLFW.getCursorPos window
  startCaptureEvents window width height eventsMVar

--setCursorInputMode win CursorInputMode'Hidden
--pure $ CursorPos $ (round x, round y)

withWindow :: Int -> Int -> [Char] -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  Just _mon <- getPrimaryMonitor
  let fullscreen = Nothing -- Just mon
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

renderGame :: Window -> Picture -> IO ()
renderGame window picture = do
  -- (width, height) <- getWindowSize window
  let (width, height) = (640, 480) :: (Int, Int)
  GL.matrixMode $= GL.Projection
  GL.preservingMatrix $ do
    -- setup the co-ordinate system
    GL.loadIdentity
    GL.ortho 0 (fromIntegral width) (fromIntegral height) 0 0 1

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
    drawPicture picture
    void $ error . show <$> get GLU.errors
    swapBuffers window

    GL.matrixMode $= GL.Projection

  GL.matrixMode $= GL.Modelview 0

type Color = GL.Color4 Float

--------------------------------------
data Texture = Texture (Int, Int) GL.TextureObject
  deriving (Show, Eq)

data TexturePlacement = TexturePlacement Double Double
  deriving (Show, Eq)

data Picture
  = TexturePlacements Texture Double Double [TexturePlacement]
  | Pictures [Picture]
  deriving (Show, Eq)

-- | Abstract 32-bit RGBA bitmap data.
data BitmapData = BitmapData
  { bitmapSize :: (Int, Int),
    bitmapPointer :: (ForeignPtr Word8)
  }
  deriving (Show, Eq)

drawPicture :: Picture -> IO ()
drawPicture picture =
  {-# SCC "drawComponent" #-}
  case picture of
    Pictures ps ->
      mapM_ drawPicture ps
    TexturePlacements (Texture (int2Double -> width, int2Double -> height) texture) xs ys placements -> do
      -- Set up wrap and filtering mode
      GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
      GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
      GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)

      -- Enable texturing
      GL.texture GL.Texture2D $= GL.Enabled
      GL.textureFunction $= GL.Combine

      oldColor <- get GL.currentColor
      GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0
      GL.textureBinding GL.Texture2D $= Just texture
      GL.blend $= GL.Enabled
      GL.renderPrimitive GL.Quads
        $ for_ placements
        $ \(TexturePlacement xd yd) -> do
          let corners = [(0, 0), (0, 1), (1, 1), (1, 0)] :: [(Double, Double)]
          forM_ corners $ \(x, y) -> do
            GL.texCoord $ GL.TexCoord2 @GL.GLfloat (gl x) (gl y) -- remember 1 makes this match the size of the vertex/quad
            GL.vertex $ GL.Vertex2 @GL.GLfloat (gl $ (x * xs * width) + xd) (gl $ (y * ys * height) + yd)
--      GL.blend $= GL.Enabled
      --GL.primitiveRestart -- crashes with exception saying function doesnt exist

      GL.currentColor $= oldColor
      GL.texture GL.Texture2D $= GL.Disabled
  where
    gl :: Double -> GL.GLfloat
    gl = unsafeCoerce . double2Float

loadIntoOpenGL :: FilePath -> IO Texture
loadIntoOpenGL file = do
  readPng file >>= \case
    Right (ImageRGBA8 (Image width height dat)) -> unsafeWith dat $ \ptr -> do
      let txSize = GL.TextureSize2D (unsafeCoerce width) (unsafeCoerce height)
      [texture] <- GL.genObjectNames 1
      GL.textureBinding GL.Texture2D $= Just texture
      GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 txSize 0 $ GL.PixelData GL.RGBA GL.UnsignedByte ptr
      pure $ Texture (width, height) texture
    _ -> error "loadIntoOpenGL error: We currently only support png graphic files JuicyPixles reads as ImageRGBA8."
