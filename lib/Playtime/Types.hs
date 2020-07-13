{-# OPTIONS_GHC -fno-warn-orphans #-}

module Playtime.Types where

import Codec.Picture.Types (Image, PixelRGBA8)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import GHC.Num
import qualified Graphics.Rendering.OpenGL.GL as GL (TextureObject)
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Prelude
import Playtime.Textures

-- Event Types
data Color = RGBA Int Int Int Int deriving (Eq, Ord, Show, Generic, NFData)

data Event
  = RenderEvent SystemTime
  | MouseEvent GLFW.MouseButton GLFW.MouseButtonState
  | KeyEvent GLFW.Key GLFW.KeyState
  | CursorPosEvent Double Double
  | WindowSizeEvent Int Int
  | WindowCloseEvent
  deriving (Show)

-- Game State Types

gameExitRequested :: EngineState -> Bool
gameExitRequested es = Exit `elem` (gsActions es)

data OneTimeEffect' = Load | Save | Reset deriving (Eq, Ord, Show, Generic, NFData)

data MovementAction' = Up | Down | Left' | Right' deriving (Eq, Ord, Show, Generic, NFData)

data Action = OneTimeEffect OneTimeEffect' | Exit | MovementAction MovementAction' deriving (Eq, Ord, Show, Generic, NFData)

oneTimeEffectMay :: Action -> Maybe OneTimeEffect'
oneTimeEffectMay (OneTimeEffect v) = Just v
oneTimeEffectMay _ = Nothing

movementAction :: Action -> Maybe MovementAction'
movementAction (MovementAction v) = Just v
movementAction _ = Nothing

data EngineState = EngineState
  { gsCursorPos :: Pos,
    gsFps :: Double,
    gsLogicalDimensions :: Dimensions,
    gsKeysPressed :: Set GLFW.Key,
    gsMousePressed :: Set GLFW.MouseButton,
    gsLastLoopTime :: SystemTime,
    gsActions :: Set Action,
    gsTimes :: [Integer],
    gsWindowSize :: Dimensions
  }
  deriving (Show, Generic, NFData)

newtype Board = Board {unBoard :: Map Pos TextureId} deriving newtype (Show, Semigroup, Monoid, NFData)

instance FromJSON Board where parseJSON = fmap (Board . mapFromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . mapToList . unBoard

data UIMode = TexturePlacementMode TextureId | TextureMoveMode deriving (Show, Generic, NFData, ToJSON, FromJSON)

data GameState = GameState
  { gsUIMode :: UIMode,
    gsCollisions :: (Maybe Area, Maybe Area, Maybe Area, Maybe Area),
    gsFloor :: Board,
    gsRoom :: Board,
    gsLastPlacement :: Pos,
    gsMainCharacterPosition :: Pos
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

-- Textures Types
data Texture = Texture
  { tDimensions :: Dimensions,
    tGLObject :: GL.TextureObject,
    tImage :: Image PixelRGBA8
  }

data FillType = Solid | Border Float deriving (Show, Eq, Generic, NFData)

data TexturePlacements
  = Rectangle FillType Area Color
  | TexturePlacements TextureId Area
  deriving (Show, Eq, Generic, NFData)

data Pos = Pos {x :: Double, y :: Double} deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

data Scale = Scale {sx :: Double, sy :: Double} deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

data Dimensions = Dimensions {width :: Double, height :: Double} deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

data Area = Area Pos Dimensions deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

(|*|) :: Scale -> Dimensions -> Dimensions
(|*|) Scale {sx, sy} Dimensions {width, height} = Dimensions {width = width * sx, height = height * sy}

(|+|) :: Pos -> Dimensions -> Pos
(|+|) Pos {x, y} Dimensions {width, height} = Pos {x = x + width, y = y + height}

(|/|) :: Dimensions -> Dimensions -> Scale
(|/|) Dimensions {width = w1, height = h1} Dimensions {width = w2, height = h2} = Scale {sx = w1 `divideDouble` w2, sy = h1 `divideDouble` h2}

isWithin :: Pos -> Area -> Bool
isWithin (Pos cx cy) (Area (Pos x y) (Dimensions width height)) = x <= cx && y <= cy && cx < (x + width) && cy < (y + height)

collidesWith :: Area -> Area -> Bool
collidesWith area1 area2 = any (`isWithin` area2) $ cornersList area1

cornerScales :: (Scale, Scale, Scale, Scale)
cornerScales = (Scale 0 0, Scale 0 1, Scale 1 1, Scale 1 0)

corners :: Area -> (Pos, Pos, Pos, Pos)
corners (Area pos dim) = case cornerScales of (c1, c2, c3, c4) -> (f c1, f c2, f c3, f c4)
  where
    f scale = pos |+| (scale |*| (dim -1))

cornersGL :: Area -> (Pos, Pos, Pos, Pos)
cornersGL (Area pos dim) = case cornerScales of (c1, c2, c3, c4) -> (f c1, f c2, f c3, f c4)
  where
    f scale = pos |+| (scale |*| dim)

cornersList :: Area -> [Pos]
cornersList area = case corners area of (c1, c2, c3, c4) -> [c1, c2, c3, c4]

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
