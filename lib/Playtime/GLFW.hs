module Playtime.GLFW
  ( withGLFW,
    setEventCallback,
  )
where

import GHC.Err (error)
import GHC.Float (double2Int)
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Extra
import My.IO
import My.Prelude
import Playtime.Types

withGLFW :: Dim -> [Char] -> (GLFW.Window -> IO ()) -> IO ()
withGLFW (Relative width, Relative height) title glCode = do
  GLFW.setErrorCallback $ Just $ \e -> error . ("GLFW:" <>) . (show e <>)
  whenM GLFW.init $ flip finally GLFW.terminate $ do
    Just _mon <- GLFW.getPrimaryMonitor
    let fullscreen = Nothing -- Just mon
    GLFW.createWindow (double2Int width) (double2Int height) title fullscreen Nothing >>= \case
      Just window -> flip finally (GLFW.destroyWindow window) $ do
        GLFW.makeContextCurrent $ Just window
        -- setCursorInputMode win CursorInputMode'Hidden
        glCode window
      Nothing -> error "createWindow returned Nothing"

setEventCallback :: GLFW.Window -> (Event -> IO ()) -> IO ()
setEventCallback window handleEvent = do
  GLFW.setMouseButtonCallback window $ Just $ \_ button state _modifiers -> handleEvent $ MouseEvent button state
  GLFW.setKeyCallback window $ Just $ \_ key _scancode keyState _modifiers -> handleEvent $ KeyEvent key keyState
  GLFW.setWindowSizeCallback window $ Just $ \_ width height -> handleEvent $ WindowSizeEvent width height
  GLFW.setCursorPosCallback window $ Just $ \_ x y -> handleEvent $ CursorPosEvent (Absolute x, Absolute y)
  GLFW.setWindowCloseCallback window $ Just $ \_ -> handleEvent $ WindowCloseEvent
