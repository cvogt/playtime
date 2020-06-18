module GLFWHelpers where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.IO
import My.Prelude

newtype GLFWCursorPosition = GLFWCursorPosition {unGLFWCursorPosition :: (Double, Double)} deriving (Show)

data MouseEvent = MouseEvent
  { meButton :: MouseButton,
    meButtonState :: MouseButtonState,
    meModifierKeys :: ModifierKeys,
    meCursorPosition :: GLFWCursorPosition
  }
  deriving (Show)

data CapturedInput = CapturedInput
  { cieMouse :: [MouseEvent]
  }
  deriving (Show)

emptyCapturedInput :: CapturedInput
emptyCapturedInput = CapturedInput []

startCaptureEvents :: Window -> MVar CapturedInput -> IO ()
startCaptureEvents win mvar = do
  setMouseButtonCallback win $ Just $ \_ mb mbs mk -> do
    (x, y) <- GLFW.getCursorPos win
    modifyMVar_ mvar $ \ci -> pure ci {cieMouse = MouseEvent mb mbs mk (GLFWCursorPosition (x, y)) : cieMouse ci}
