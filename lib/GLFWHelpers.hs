module GLFWHelpers where

import Data.List (unwords)
import Data.List (reverse)
import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.IO
import My.Prelude

newtype CursorPos = CursorPos {unCursorPos :: (Double, Double)} deriving (Show)

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
  deriving (Show)

startCaptureEvents :: Window -> MVar [InputEvent] -> IO ()
startCaptureEvents win mvar = do
  setMouseButtonCallback win $ Just $ \_ button state modifiers -> do
    (x, y) <- GLFW.getCursorPos win
    modifyMVar_ mvar $ pure . ((MouseEvent' $ MouseEvent button state modifiers $ CursorPos (x, y)) :)
  setKeyCallback win $ Just $ \_ key _scancode keyState modifiers -> do
    modifyMVar_ mvar $ pure . ((KeyEvent' $ KeyEvent key keyState modifiers) :)
  setCursorPosCallback win $ Just $ \_ x y -> do
    modifyMVar_ mvar $ pure . ((CursorPosEvent' $ CursorPosEvent $ CursorPos (x, y)) :)

fetchEvents :: MVar [InputEvent] -> IO [InputEvent]
fetchEvents eventsMVar = do
  pollEvents
  capturedInputs <- modifyMVar eventsMVar $ \cs -> pure ([], cs)
  time <- getSystemTime
  pure $ reverse $ GameLoopEvent time : capturedInputs

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
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
      Nothing -> pure ()
    GLFW.terminate
  where
    simpleErrorCallback e s =
      putStrLn $ unwords [show e, show s]
