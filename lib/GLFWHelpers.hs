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

data KeyEvent = KeyEvent
  { keKey :: Key,
    keKeyState :: KeyState,
    keModifierKeys :: ModifierKeys
  }
  deriving (Show)

data InputEvent
  = GameLoopEvent SystemTime
  | MouseEvent' MouseEvent
  | KeyEvent' KeyEvent
  deriving (Show)

emptyCapturedInput :: [InputEvent]
emptyCapturedInput = []

startCaptureEvents :: Window -> MVar [InputEvent] -> IO ()
startCaptureEvents win mvar = do
  setMouseButtonCallback win $ Just $ \_ button state modifiers -> do
    (x, y) <- GLFW.getCursorPos win
    modifyMVar_ mvar $ pure . ((MouseEvent' $ MouseEvent button state modifiers $ GLFWCursorPosition (x, y)) :)
  setKeyCallback win $ Just $ \_ key _scancode keyState modifiers -> do
    modifyMVar_ mvar $ pure . ((KeyEvent' $ KeyEvent key keyState modifiers) :)

withWindow :: Int -> Int -> [Char] -> MVar [InputEvent] -> (GLFW.Window -> IO ()) -> IO ()
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
