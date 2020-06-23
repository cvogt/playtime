module SpaceMiner.Types where

import qualified Graphics.Rendering.OpenGL.GL as GL (TextureObject)
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude

-- Event Types
newtype CursorPos = CursorPos {unCursorPos :: (Double, Double)} deriving (Eq, Ord, Show)

data MouseEvent = MouseEvent
  { meButton :: MouseButton,
    meButtonState :: MouseButtonState,
    meModifierKeys :: ModifierKeys
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
    gsKeysPressed :: Set Key,
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
