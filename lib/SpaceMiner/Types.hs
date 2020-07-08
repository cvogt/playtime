{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpaceMiner.Types where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
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

data Event
  = GameLoopEvent SystemTime
  | MouseEvent GLFW.MouseButton GLFW.MouseButtonState
  | KeyEvent GLFW.Key GLFW.KeyState
  | CursorPosEvent Pos
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
gameExitRequested (GameState GenericGameState {gsInputActions} _ _) = OneTimeAction Exit `elem` gsInputActions

data OneTimeAction' = Load | Save | Exit | Reset deriving (Eq, Ord, Generic, NFData)

data MovementAction' = Up | Down | Left' | Right' deriving (Eq, Ord, Generic, NFData)

data InputAction = OneTimeAction OneTimeAction' | MovementAction MovementAction' deriving (Eq, Ord, Generic, NFData)

oneTimeAction :: InputAction -> Maybe OneTimeAction'
oneTimeAction (OneTimeAction v) = Just v
oneTimeAction _ = Nothing

movementAction :: InputAction -> Maybe MovementAction'
movementAction (MovementAction v) = Just v
movementAction _ = Nothing

data Mode = PlacementMode | DeleteMode deriving (Eq, Ord, Generic, NFData)

class Has a b where get :: a -> b

instance Has GameState GenericGameState where get = gsGenericGameState

instance Has GameState TransientGameState where get = gsTransientGameState

instance Has GameState PersistentGameState where get = gsPersistentGameState

data GenericGameState = GenericGameState
  { gsCursorPos :: Pos,
    gsFps :: Double,
    gsKeysPressed :: Set GLFW.Key,
    gsLastLoopTime :: SystemTime,
    gsInputActions :: Set InputAction,
    gsTimes :: [Integer]
  }
  deriving (Generic, NFData)

data TransientGameState = TransientGameState
  { gsLastPlacement :: Pos,
    gsModes :: Set Mode
  }
  deriving (Generic, NFData)

newtype Board = Board {unBoard :: Map Pos TextureId} deriving newtype (Semigroup, Monoid, NFData)

instance FromJSON Board where parseJSON = fmap (Board . mapFromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . mapToList . unBoard

data PersistentGameState = PersistentGameState
  { gsActiveTile :: TextureId,
    gsBoard :: Board,
    gsMainCharacterPosition :: Pos
  }
  deriving (Generic, NFData, ToJSON, FromJSON)

-- Textures Types
data Texture = Texture (Int, Int) GL.TextureObject deriving (Show, Eq)

data TexturePlacements = TexturePlacements TextureId Double Double (NonEmpty Pos) deriving (Show, Eq, Generic, NFData)
