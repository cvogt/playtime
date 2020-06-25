{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpaceMiner.Types where

import qualified Graphics.Rendering.OpenGL.GL as GL (TextureObject)
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Prelude
import SpaceMiner.Textures

data Dimensions = Dimensions
  { width :: Int,
    height :: Int
  }

data ScaleInt = ScaleInt Int

-- Event Types
data Pos = Pos Double Double deriving (Eq, Ord, Show, Generic, NFData)

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

data CursorPosEvent = CursorPosEvent Pos deriving (Show)

data Event
  = GameLoopEvent SystemTime
  | MouseEvent' MouseEvent
  | KeyEvent' KeyEvent
  | CursorPosEvent' CursorPosEvent
  | WindowCloseEvent
  deriving (Show)

-- Game State Types

data GameStateStrict = GameStateStrict

data GameState = GameState
  { gsBoard :: Map Pos TextureId,
    gsPlacementMode :: Bool,
    gsDeleteMode :: Bool,
    gsKeysPressed :: Set GLFW.Key,
    gsMainCharacterPosition :: Pos,
    gsActiveTile :: TextureId,
    gsExitGame :: Bool,
    gsTimes :: [Integer],
    gsFps :: Double,
    gsCursorPos :: Pos,
    gsLastPlacement :: Pos,
    gsLastLoopTime :: SystemTime
  }
  deriving (Generic, NFData)

-- Textures Types
data Texture = Texture (Int, Int) GL.TextureObject deriving (Show, Eq)

data TexturePlacements = TexturePlacements TextureId Double Double (NonEmpty Pos) deriving (Show, Eq, Generic, NFData)
