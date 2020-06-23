module GLFWHelpers where

import qualified Data.Map as Map
import GHC.Float (double2Float, int2Double)
import GHC.Real ((/), fromIntegral)
import Graphics.Rendering.OpenGL (($=), get)
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU.Errors as GLU
import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.IO
import My.Extra
import My.Prelude
import SpaceMiner.MutableState
import SpaceMiner.Textures
import SpaceMiner.Types
import Codec.Picture (readPng, DynamicImage(ImageRGBA8), Image(Image))
import GHC.Err (error)
import Data.Vector.Storable (unsafeWith)

withGLFW :: Int -> Int -> [Char] -> (GLFW.Window -> MutableState -> (TextureId -> Texture) -> IO ()) -> IO ()
withGLFW width height title glCode = do
  GLFW.setErrorCallback $ Just $ \e -> error . ("GLFW:" <>) . (show e <>)
  whenM GLFW.init $ flip finally GLFW.terminate $ do
    Just _mon <- getPrimaryMonitor
    let fullscreen = Nothing -- Just mon
    GLFW.createWindow width height title fullscreen Nothing >>= \case
      Just window -> flip finally (GLFW.destroyWindow window) $ do
        GLFW.makeContextCurrent $ Just window
        mutableState <- initialMutableState
        startCaptureEvents window width height mutableState
        -- setCursorInputMode win CursorInputMode'Hidden
        glCode window mutableState =<< loadTextures
      Nothing -> error "createWindow returned Nothing"

startCaptureEvents :: Window -> Int -> Int -> MutableState -> IO ()
startCaptureEvents window (int2Double -> logicWidth) (int2Double -> logicHeight) MutableState{msEventsMVar=mvar} = do
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

renderGame :: Window -> [Visualization] -> IO ()
renderGame window visualizations = do
  (width, height) <- getWindowSize window
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 (fromIntegral width) (fromIntegral height) 0 0 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  GL.matrixMode $= GL.Modelview 0
  -- GL.lineSmooth $= GL.Disabled

  -- enable png alpha channel transparancy
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  checkErrorsGLU "before"

  GL.texture GL.Texture2D $= GL.Enabled
  for_ visualizations $ \(TexturePlacements (Texture (int2Double -> twidth, int2Double -> theight) texture) xs ys placements) -> do
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureBinding GL.Texture2D $= Just texture
    GL.renderPrimitive GL.Quads $ for_ placements $ \(TexturePlacement xd yd) -> do
      forM_ [(0, 0), (0, 1), (1, 1), (1, 0)] $ \(x, y) -> do
        GL.texCoord $ GL.TexCoord2 (double2Float x) (double2Float y) -- remember 1 makes this match the size of the vertex/quad
        GL.vertex $ GL.Vertex2 (double2Float $ (x * xs * twidth) + xd) (double2Float $ (y * ys * theight) + yd)

  checkErrorsGLU "after"

  swapBuffers window

  where
    checkErrorsGLU msg = void $ error . ("GLU.errors " <>).(msg <>).( ": " <>) . show <$> get GLU.errors

loadTextures :: IO (TextureId -> Texture)
loadTextures = do
  m' <- sequence $ loadIntoOpenGL <$> textureNameMap
  pure $ \key -> fromMaybe (error $"failed to load texture: "<> show key) $ Map.lookup key m'
  where
    loadIntoOpenGL :: FilePath -> IO Texture
    loadIntoOpenGL name = do
      readPng (assetsDir </>name<>".png") >>= \case
        Right (ImageRGBA8 (Image width height dat)) -> unsafeWith dat $ \ptr -> do
          let txSize = GL.TextureSize2D (fromIntegral width) (fromIntegral height)
          [texture] <- GL.genObjectNames 1
          GL.textureBinding GL.Texture2D $= Just texture
          GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 txSize 0 $ GL.PixelData GL.RGBA GL.UnsignedByte ptr
          pure $ Texture (width, height) texture
        _ -> error "loadIntoOpenGL error: We currently only support png graphic files JuicyPixles reads as ImageRGBA8."
