module GLFWHelpers where

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

renderGame :: Window -> [Visualization] -> IO ()
renderGame window visualizations = do
  (width, height) <- getWindowSize window
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 (fromIntegral width) (fromIntegral height) 0 0 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0

  GL.preservingMatrix $ do
    GL.matrixMode $= GL.Modelview 0

    GL.lineSmooth $= GL.Disabled
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    void $ error . show <$> get GLU.errors
    for_ visualizations $ \(TexturePlacements (Texture (int2Double -> twidth, int2Double -> theight) texture) xs ys placements) -> do
      GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
      GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
      GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)

      GL.texture GL.Texture2D $= GL.Enabled
      GL.textureFunction $= GL.Combine

      GL.textureBinding GL.Texture2D $= Just texture
      GL.blend $= GL.Enabled
      GL.renderPrimitive GL.Quads
        $ for_ placements
        $ \(TexturePlacement xd yd) -> do
          let corners = [(0, 0), (0, 1), (1, 1), (1, 0)] :: [(Double, Double)]
          forM_ corners $ \(x, y) -> do
            GL.texCoord $ GL.TexCoord2 (double2Float x) (double2Float y) -- remember 1 makes this match the size of the vertex/quad
            GL.vertex $ GL.Vertex2 (double2Float $ (x * xs * twidth) + xd) (double2Float $ (y * ys * theight) + yd)
      --GL.primitiveRestart -- crashes with exception saying function doesnt exist

      GL.texture GL.Texture2D $= GL.Disabled

    void $ error . show <$> get GLU.errors
    swapBuffers window

type Color = GL.Color4 Float

--------------------------------------
data Texture = Texture (Int, Int) GL.TextureObject
  deriving (Show, Eq)

data TexturePlacement = TexturePlacement Double Double
  deriving (Show, Eq)

data Visualization = TexturePlacements Texture Double Double [TexturePlacement]
  deriving (Show, Eq)

-- | Abstract 32-bit RGBA bitmap data.
data BitmapData = BitmapData
  { bitmapSize :: (Int, Int),
    bitmapPointer :: (ForeignPtr Word8)
  }
  deriving (Show, Eq)

loadIntoOpenGL :: FilePath -> IO Texture
loadIntoOpenGL file = do
  readPng file >>= \case
    Right (ImageRGBA8 (Image width height dat)) -> unsafeWith dat $ \ptr -> do
      let txSize = GL.TextureSize2D (fromIntegral width) (fromIntegral height)
      [texture] <- GL.genObjectNames 1
      GL.textureBinding GL.Texture2D $= Just texture
      GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 txSize 0 $ GL.PixelData GL.RGBA GL.UnsignedByte ptr
      pure $ Texture (width, height) texture
    _ -> error "loadIntoOpenGL error: We currently only support png graphic files JuicyPixles reads as ImageRGBA8."
