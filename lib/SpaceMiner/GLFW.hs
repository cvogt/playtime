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
import SpaceMiner.GL
import SpaceMiner.MutableState
import SpaceMiner.Textures
import SpaceMiner.Types

withGLFW :: Dimensions -> ScaleInt -> [Char] -> (GLFW.Window -> MutableState -> (TextureId -> Texture) -> IO ()) -> IO ()
withGLFW logicalDimensions@Dimensions {width, height} (ScaleInt windowScalingFactor) title glCode = do
  GLFW.setErrorCallback $ Just $ \e -> error . ("GLFW:" <>) . (show e <>)
  whenM GLFW.init $ flip finally GLFW.terminate $ do
    Just _mon <- GLFW.getPrimaryMonitor
    let fullscreen = Nothing -- Just mon
    GLFW.createWindow (windowScalingFactor * width) (height * windowScalingFactor) title fullscreen Nothing >>= \case
      Just window -> flip finally (GLFW.destroyWindow window) $ do
        GLFW.makeContextCurrent $ Just window
        mutableState <- initialMutableState
        startCaptureEvents window logicalDimensions mutableState
        -- setCursorInputMode win CursorInputMode'Hidden
        glCode window mutableState =<< loadTextures
      Nothing -> error "createWindow returned Nothing"

startCaptureEvents :: GLFW.Window -> Dimensions -> MutableState -> IO ()
startCaptureEvents window Dimensions {width = logicWidth, height = logicHeight} MutableState {msEventsMVar = mvar} = do
  GLFW.setMouseButtonCallback window $ Just $ \_ button state modifiers -> do
    modifyMVar_ mvar $ pure . ((MouseEvent' $ MouseEvent button state modifiers) :)
  GLFW.setKeyCallback window $ Just $ \_ key _scancode keyState modifiers ->
    modifyMVar_ mvar $ pure . ((KeyEvent' $ KeyEvent key keyState modifiers) :)
  GLFW.setCursorPosCallback window $ Just $ \window' x y -> do
    -- this ratio calculation leads to proper relative scaling on window resize
    -- FIXME: we still get distortion if aspect ration of resized window is different
    --        we should be able to fix that by adding black borders as needed
    (actualWidth, actualHeight) <- GLFW.getWindowSize window'
    let w = int2Double actualWidth / int2Double logicWidth
        h = int2Double actualHeight / int2Double logicHeight
    modifyMVar_ mvar $ pure . ((CursorPosEvent' $ CursorPosEvent $ Pos (x / w) (y / h)) :)
  GLFW.setWindowCloseCallback window $ Just $ \_ ->
    modifyMVar_ mvar $ pure . (WindowCloseEvent :)
