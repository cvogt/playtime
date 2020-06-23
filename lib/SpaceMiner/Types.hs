module SpaceMiner.Types where

import qualified Graphics.Rendering.OpenGL.GL as GL (TextureObject)
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Prelude

-- Event Types
newtype CursorPos = CursorPos {unCursorPos :: (Double, Double)} deriving (Eq, Ord, Show)

data MouseEvent = MouseEvent
  { meButton :: GLFW.MouseButton,
    meButtonState :: GLFW.MouseButtonState,
    meModifierKeys :: GLFW.ModifierKeys
  }
  deriving (Show)

data KeyEvent = KeyEvent
  { keKey :: GLFW.Key,
    keKeyState :: GLFW.KeyState,
    keModifierKeys :: GLFW.ModifierKeys
  }
  deriving (Show)

data CursorPosEvent = CursorPosEvent CursorPos
  deriving (Show)

data Event
  = GameLoopEvent SystemTime
  | MouseEvent' MouseEvent
  | KeyEvent' KeyEvent
  | CursorPosEvent' CursorPosEvent
  | WindowCloseEvent
  deriving (Show)

-- Game State Types

data GameState = GameState
  { gsBoard :: Set CursorPos,
    gsPlacementMode :: Bool,
    gsDeleteMode :: Bool,
    gsKeysPressed :: Set GLFW.Key,
    gsMainCharacterPosition :: CursorPos,
    gsExitGame :: Bool,
    gsTimes :: [Integer],
    gsFps :: Double,
    gsCursorPos :: CursorPos,
    gsLastPlacement :: CursorPos,
    gsLastLoopTime :: SystemTime
  }

-- Textures Types

data TexturePlacement = TexturePlacement Double Double
  deriving (Show, Eq)

data Texture = Texture (Int, Int) GL.TextureObject
  deriving (Show, Eq)

data Visualization = TexturePlacements Texture Double Double [TexturePlacement]
  deriving (Show, Eq)
