module SpaceMiner.GLFW
  ( withGLFW,
    startCaptureEvents,
  )
where

import GHC.Err (error)
import GHC.Float (int2Double)
import GHC.Real (Fractional ((/)))
import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Extra
import My.IO
import My.Prelude
import SpaceMiner.GL
import SpaceMiner.MutableState
import SpaceMiner.Textures
import SpaceMiner.Types

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
startCaptureEvents window (int2Double -> logicWidth) (int2Double -> logicHeight) MutableState {msEventsMVar = mvar} = do
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
