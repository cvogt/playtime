module GLFWHelpers where

import Control.Monad (mapM_)
import Data.List (reverse, unwords)
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

renderGame :: Window -> Picture -> IO ()
renderGame window picture = do
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
    drawPicture 1.0 picture
    void $ error . show <$> get GLU.errors
    swapBuffers window

    GL.matrixMode $= GL.Projection

  GL.matrixMode $= GL.Modelview 0

type Color = GL.Color4 Float

--------------------------------------
data Texture = Texture (Int, Int) GL.TextureObject
  deriving (Show, Eq)

data Bitmap = Bitmap Float Float Float Float Texture
  deriving (Show, Eq)

data Picture
  = Bitmap' Bitmap
  | Translate Float Float Picture
  | -- | Some text to draw with a vector font.
    Text Float Float Float Float Color String
  | Pictures [Picture]
  | Bitmaps [Picture]
  | -- | A picture scaled by the given x and y factors.
    Scale Float Float Picture
  deriving (Show, Eq)

-- | Abstract 32-bit RGBA bitmap data.
data BitmapData = BitmapData
  { bitmapSize :: (Int, Int),
    bitmapPointer :: (ForeignPtr Word8)
  }
  deriving (Show, Eq)

drawPicture :: Float -> Picture -> IO ()
drawPicture circScale picture =
  {-# SCC "drawComponent" #-}
  case picture of
    Pictures ps ->
      mapM_ (drawPicture circScale) ps
    Bitmaps ps ->
      mapM_ (drawPicture circScale) ps
    -- stroke text
    --      text looks weird when we've got blend on,
    --      so disable it during the renderString call.
    Text xs ys xd yd col str ->
      GL.preservingMatrix $ do
        GL.translate (GL.Vector3 (unsafeCoerce xd) (unsafeCoerce yd :: GL.GLfloat) 0)
        GL.preservingMatrix $ do
          GL.scale (unsafeCoerce xs) (unsafeCoerce ys :: GL.GLfloat) 1
          oldColor <- get GL.currentColor
          GL.currentColor $= col
          GL.blend $= GL.Disabled
          GL.preservingMatrix $ GLUT.renderString GLUT.Roman str
          GL.blend $= GL.Enabled
          GL.currentColor $= oldColor
    Translate tx ty p ->
      GL.preservingMatrix $
        do
          GL.translate (GL.Vector3 (unsafeCoerce tx) (unsafeCoerce ty :: GL.GLfloat) 0)
          drawPicture circScale p
    Scale sx sy p ->
      GL.preservingMatrix $
        do
          GL.scale (unsafeCoerce sx) (unsafeCoerce sy :: GL.GLfloat) 1
          let mscale = max sx sy
          drawPicture (circScale * mscale) p
    Bitmap' (Bitmap _xs _ys xd yd (Texture xy tex)) -> do
      let (fromIntegral -> width, fromIntegral -> height) = xy

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
      let corners = [(0, 0), (0, 1), (1, 1), (1, 0)] :: [(Float, Float)]
      GL.renderPrimitive GL.Polygon $ forM_ corners $ \(x, y) -> do
        GL.texCoord $ GL.TexCoord2 @GL.GLfloat (unsafeCoerce x) (unsafeCoerce y)
        GL.vertex $ GL.Vertex2 @GL.GLfloat ((unsafeCoerce $ x * width) + xd) ((unsafeCoerce $ y * height) + yd)

      -- Restore color
      GL.currentColor $= oldColor

      -- Disable texturing
      GL.texture GL.Texture2D $= GL.Disabled

sendTextureToGL :: BitmapData -> IO GL.TextureObject
sendTextureToGL (BitmapData (width, height) fptr) = do
  let txSize = GL.TextureSize2D (unsafeCoerce width) (unsafeCoerce height)
  [texture] <- GL.genObjectNames 1
  GL.textureBinding GL.Texture2D $= Just texture
  withForeignPtr fptr $ GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 txSize 0 . GL.PixelData GL.RGBA GL.UnsignedByte
  pure texture
