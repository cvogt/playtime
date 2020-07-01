{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpaceMiner.Types where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Map as Map
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
data Pos = Pos Double Double deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

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

data GameState = GameState
  { gsGenericGameState :: GenericGameState,
    gsTransientGameState :: TransientGameState,
    gsPersistentGameState :: PersistentGameState
  }
  deriving (Generic, NFData)

gameExitRequested :: GameState -> Bool
gameExitRequested (GameState GenericGameState {gsRequestedExitGame} _ _) = gsRequestedExitGame

data GenericGameState = GenericGameState
  { gsCursorPos :: Pos,
    gsFps :: Double,
    gsKeysPressed :: Set GLFW.Key,
    gsLastLoopTime :: SystemTime,
    gsRequestedExitGame :: Bool,
    gsRequestedLoadGame :: Bool,
    gsRequestedResetGame :: Bool,
    gsRequestedSaveGame :: Bool,
    gsTimes :: [Integer]
  }
  deriving (Generic, NFData)

data TransientGameState = TransientGameState
  { gsDeleteMode :: Bool,
    gsLastPlacement :: Pos,
    gsPlacementMode :: Bool
  }
  deriving (Generic, NFData)

newtype Board = Board {unBoard :: Map Pos TextureId} deriving newtype (Semigroup, Monoid, NFData)

instance FromJSON Board where parseJSON = fmap (Board . Map.fromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . Map.toList . unBoard

data PersistentGameState = PersistentGameState
  { gsActiveTile :: TextureId,
    gsBoard :: Board,
    gsMainCharacterPosition :: Pos
  }
  deriving (Generic, NFData, ToJSON, FromJSON)

-- Textures Types
data Texture = Texture (Int, Int) GL.TextureObject deriving (Show, Eq)

data TexturePlacements = TexturePlacements TextureId Double Double (NonEmpty Pos) deriving (Show, Eq, Generic, NFData)
