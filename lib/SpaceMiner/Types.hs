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
gameExitRequested (GameState GenericGameState {gsActions} _ _) = OneTimeEffect Exit `elem` gsActions

data OneTimeEffect' = Load | Save | Exit | Reset deriving (Eq, Ord, Generic, NFData)

data MovementAction' = Up | Down | Left' | Right' deriving (Eq, Ord, Generic, NFData)

data Action = OneTimeEffect OneTimeEffect' | MovementAction MovementAction' deriving (Eq, Ord, Generic, NFData)

outputEventMay :: Action -> Maybe OneTimeEffect'
outputEventMay (OneTimeEffect v) = Just v
outputEventMay _ = Nothing

movementAction :: Action -> Maybe MovementAction'
movementAction (MovementAction v) = Just v
movementAction _ = Nothing

data Mode = PlacementMode | DeleteMode deriving (Eq, Ord, Generic, NFData)

class Has a b where
  get :: a -> b
  set :: a -> b -> a
  update :: a -> (b -> b) -> a
  update a f = set a $ f $ get a

instance Has GameState GenericGameState where
  get = gsGenericGameState
  set gs b = gs{ gsGenericGameState = b}

instance Has GameState TransientGameState where
  get = gsTransientGameState
  set gs b = gs{ gsTransientGameState = b}

instance Has GameState PersistentGameState where
  get = gsPersistentGameState
  set gs b = gs{ gsPersistentGameState = b}

data GenericGameState = GenericGameState
  { gsCursorPos :: Pos,
    gsFps :: Double,
    gsKeysPressed :: Set GLFW.Key,
    gsLastLoopTime :: SystemTime,
    gsActions :: Set Action,
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
