module SpaceMiner.GLFW
  ( withGLFW,
    startCaptureEvents,
  )
where

import GHC.Err (error)
import GHC.Float (int2Double)
import GHC.Real (Fractional ((/)))
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Extra
import My.IO
import My.Prelude
import SpaceMiner.ConcurrentState
import SpaceMiner.Types

withGLFW :: Dimensions -> ScaleInt -> [Char] -> (GLFW.Window -> IO ()) -> IO ()
withGLFW Dimensions {width, height} (ScaleInt windowScalingFactor) title glCode = do
  GLFW.setErrorCallback $ Just $ \e -> error . ("GLFW:" <>) . (show e <>)
  whenM GLFW.init $ flip finally GLFW.terminate $ do
    Just _mon <- GLFW.getPrimaryMonitor
    let fullscreen = Nothing -- Just mon
    GLFW.createWindow (windowScalingFactor * width) (height * windowScalingFactor) title fullscreen Nothing >>= \case
      Just window -> flip finally (GLFW.destroyWindow window) $ do
        GLFW.makeContextCurrent $ Just window
        -- setCursorInputMode win CursorInputMode'Hidden
        glCode window
      Nothing -> error "createWindow returned Nothing"

startCaptureEvents :: GLFW.Window -> Dimensions -> ConcurrentState -> IO ()
startCaptureEvents window Dimensions {width = logicWidth, height = logicHeight} ConcurrentState {csEvents} = do
  GLFW.setMouseButtonCallback window $ Just $ \_ button state _modifiers -> do
    modifyMVar_ csEvents $ pure . (MouseEvent button state :)
  GLFW.setKeyCallback window $ Just $ \_ key _scancode keyState _modifiers ->
    modifyMVar_ csEvents $ pure . (KeyEvent key keyState :)
  GLFW.setCursorPosCallback window $ Just $ \window' x y -> do
    -- this ratio calculation leads to proper relative scaling on window resize
    -- FIXME: we still get distortion if aspect ration of resized window is different
    --        we should be able to fix that by adding black borders as needed
    (actualWidth, actualHeight) <- GLFW.getWindowSize window'
    let w = int2Double actualWidth / int2Double logicWidth
        h = int2Double actualHeight / int2Double logicHeight
    modifyMVar_ csEvents $ pure . ((CursorPosEvent $ Pos (x / w) (y / h)) :)
  GLFW.setWindowCloseCallback window $ Just $ \_ ->
    modifyMVar_ csEvents $ pure . (WindowCloseEvent :)
