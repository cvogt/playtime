{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpaceMiner.Types where

import Codec.Picture.Types (Image, PixelRGBA8)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import GHC.Num
import qualified Graphics.Rendering.OpenGL.GL as GL (TextureObject)
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Prelude
import SpaceMiner.Textures

-- Event Types
data Color = RGBA Int Int Int Int deriving (Eq, Ord, Show, Generic, NFData)

data Event
  = RenderEvent SystemTime
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
  deriving (Show, Generic, NFData)

gameExitRequested :: GameState -> Bool
gameExitRequested (GameState GenericGameState {gsActions} _ _) = Exit `elem` gsActions

data OneTimeEffect' = Load | Save | Reset deriving (Eq, Ord, Show, Generic, NFData)

data MovementAction' = Up | Down | Left' | Right' deriving (Eq, Ord, Show, Generic, NFData)

data Action = OneTimeEffect OneTimeEffect' | Exit | MovementAction MovementAction' deriving (Eq, Ord, Show, Generic, NFData)

oneTimeEffectMay :: Action -> Maybe OneTimeEffect'
oneTimeEffectMay (OneTimeEffect v) = Just v
oneTimeEffectMay _ = Nothing

movementAction :: Action -> Maybe MovementAction'
movementAction (MovementAction v) = Just v
movementAction _ = Nothing

data Mode = PlacementMode | DeleteMode deriving (Eq, Ord, Show, Generic, NFData)

class Has a b where
  get :: a -> b
  set :: a -> b -> a
  update :: a -> (b -> b) -> a
  update a f = set a $ f $ get a

instance Has GameState GenericGameState where
  get = gsGenericGameState
  set gs b = gs {gsGenericGameState = b}

instance Has GameState TransientGameState where
  get = gsTransientGameState
  set gs b = gs {gsTransientGameState = b}

instance Has GameState PersistentGameState where
  get = gsPersistentGameState
  set gs b = gs {gsPersistentGameState = b}

data GenericGameState = GenericGameState
  { gsCursorPos :: Pos,
    gsFps :: Double,
    gsKeysPressed :: Set GLFW.Key,
    gsLastLoopTime :: SystemTime,
    gsActions :: Set Action,
    gsTimes :: [Integer]
  }
  deriving (Show, Generic, NFData)

data TransientGameState = TransientGameState
  { gsLastPlacement :: Pos,
    gsModes :: Set Mode
  }
  deriving (Show, Generic, NFData)

newtype Board = Board {unBoard :: Map Pos TextureId} deriving newtype (Show, Semigroup, Monoid, NFData)

instance FromJSON Board where parseJSON = fmap (Board . mapFromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . mapToList . unBoard

data PersistentGameState = PersistentGameState
  { gsActiveTile :: TextureId,
    gsBoard :: Board,
    gsMainCharacterPosition :: Pos
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

-- Textures Types
data Texture = Texture Dimensions GL.TextureObject (Image PixelRGBA8) deriving (Eq)

data FillType = Solid | Border Float deriving (Show, Eq, Generic, NFData)

data TexturePlacements
  = Rectangle FillType Pos Dimensions Color
  | TexturePlacements TextureId Scale Pos
  deriving (Show, Eq, Generic, NFData)

data Pos = Pos {x :: Double, y :: Double} deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

data Scale = Scale {sx :: Double, sy :: Double} deriving (Eq, Ord, Show, Generic, NFData)

data Dimensions = Dimensions {width :: Double, height :: Double} deriving (Eq, Ord, Show, Generic, NFData)

(|*|) :: Scale -> Dimensions -> Dimensions
(|*|) Scale {sx, sy} Dimensions {width, height} = Dimensions {width = width * sx, height = height * sy}

instance Num Scale where
  (Scale lx ly) + (Scale rx ry) = Scale (lx + rx) (ly + ry)
  (Scale lx ly) - (Scale rx ry) = Scale (lx - rx) (ly - ry)
  (Scale lx ly) * (Scale rx ry) = Scale (lx * rx) (ly * ry)
  negate (Scale x y) = Scale (- x) (- y)
  abs (Scale x y) = Scale (abs x) (abs y)
  signum (Scale x y) = Scale (signum x) (signum y)
  fromInteger i = Scale (fromInteger i) (fromInteger i)

instance Num Pos where
  (Pos lx ly) + (Pos rx ry) = Pos (lx + rx) (ly + ry)
  (Pos lx ly) - (Pos rx ry) = Pos (lx - rx) (ly - ry)
  (Pos lx ly) * (Pos rx ry) = Pos (lx * rx) (ly * ry)
  negate (Pos x y) = Pos (- x) (- y)
  abs (Pos x y) = Pos (abs x) (abs y)
  signum (Pos x y) = Pos (signum x) (signum y)
  fromInteger i = Pos (fromInteger i) (fromInteger i)

instance Num Dimensions where
  (Dimensions lx ly) + (Dimensions rx ry) = Dimensions (lx + rx) (ly + ry)
  (Dimensions lx ly) - (Dimensions rx ry) = Dimensions (lx - rx) (ly - ry)
  (Dimensions lx ly) * (Dimensions rx ry) = Dimensions (lx * rx) (ly * ry)
  negate (Dimensions x y) = Dimensions (- x) (- y)
  abs (Dimensions x y) = Dimensions (abs x) (abs y)
  signum (Dimensions x y) = Dimensions (signum x) (signum y)
  fromInteger i = Dimensions (fromInteger i) (fromInteger i)
