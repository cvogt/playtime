module GLFWHelpers where

import Data.List (unwords)
import Data.List (reverse)
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Rendering as GlossRendering
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
  | WindowCloseEvent
  deriving (Show)

startCaptureEvents :: Window -> MVar [InputEvent] -> IO ()
startCaptureEvents window mvar = do
  setMouseButtonCallback window $ Just $ \_ button state modifiers -> do
    (x, y) <- GLFW.getCursorPos window
    modifyMVar_ mvar $ pure . ((MouseEvent' $ MouseEvent button state modifiers $ CursorPos (x, y)) :)
  setKeyCallback window $ Just $ \_ key _scancode keyState modifiers ->
    modifyMVar_ mvar $ pure . ((KeyEvent' $ KeyEvent key keyState modifiers) :)
  setCursorPosCallback window $ Just $ \_ x y ->
    modifyMVar_ mvar $ pure . ((CursorPosEvent' $ CursorPosEvent $ CursorPos (x, y)) :)
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
  pos <- GLFW.getCursorPos window
  startCaptureEvents window eventsMVar
  --setCursorInputMode win CursorInputMode'Hidden
  pure $ CursorPos pos

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

renderGame :: Window -> GlossRendering.State -> Gloss.Picture -> IO ()
renderGame window glossState picture = do
  (w, h) <- getWindowSize window
  GlossRendering.withModelview (w, h) $ do
    GlossRendering.withClearBuffer Gloss.black $ pure ()
    GlossRendering.renderPicture glossState 1.0 picture
  swapBuffers window
