module GLFWHelpers where

import Data.List (unwords)
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

withWindow :: Int -> Int -> [Char] -> MVar CapturedInput -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title inputsMVar f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  Just _mon <- getPrimaryMonitor
  let fullscreen = Nothing -- (Just mon)
  when r $ do
    m <- GLFW.createWindow width height title fullscreen Nothing
    case m of
      (Just win) -> do
        GLFW.makeContextCurrent m
        startCaptureEvents win inputsMVar
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
      Nothing -> pure ()
    GLFW.terminate
  where
    simpleErrorCallback e s =
      putStrLn $ unwords [show e, show s]
