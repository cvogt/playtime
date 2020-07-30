{-# OPTIONS_GHC -fno-warn-orphans #-}

module Playtime.Types where

import Codec.Picture.Types (Image, PixelRGBA8)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Num
import GHC.Real
import qualified Graphics.Rendering.OpenGL.GL as GL (TextureObject)
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import My.IO
import My.Prelude

data EngineConfig = EngineConfig
  { ecDim :: Dimensions,
    ecScale :: Scale,
    ecVisualize :: EngineState -> IO [Sprite],
    ecStepGameState :: EngineState -> Event -> IO (),
    ecCheckIfContinue :: EngineState -> IO Bool,
    ecGameDebugInfo :: EngineState -> IO [[Char]]
  }

-- Event Types
data Color = RGBA Int Int Int Int deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

data Event
  = RenderEvent SystemTime
  | MouseEvent GLFW.MouseButton GLFW.MouseButtonState
  | KeyEvent GLFW.Key GLFW.KeyState
  | CursorPosEvent Pos
  | WindowSizeEvent Int Int
  | WindowCloseEvent
  deriving (Show)

-- Game State Types

gameExitRequested :: EngineState -> Bool
gameExitRequested es = Exit `elem` (esActions es)

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
  { esCursorPos :: Pos,
    esFps :: Double,
    esLogicalDimensions :: Dimensions,
    esKeysPressed :: Set GLFW.Key,
    esMousePressed :: Set GLFW.MouseButton,
    esLastLoopTime :: SystemTime,
    esActions :: Set Action,
    esTimes :: [Integer],
    esTimePassed :: Double,
    esWindowSize :: Dimensions
  }
  deriving (Show, Generic, NFData)

-- Textures Types
data Texture = Texture
  { tDimensions :: Dimensions,
    tGLObject :: GL.TextureObject,
    tImage :: Image PixelRGBA8
  }

data FillType = Solid | Border Float

data Sprite = Rectangle Area (Either Texture (FillType, Color))

spriteArea :: Sprite -> Area
spriteArea (Rectangle area _) = area

rectangle :: FillType -> Color -> Dimensions -> Pos -> Sprite
rectangle ft c d p = Rectangle (p, d) $ Right (ft, c)

rectangle' :: FillType -> Color -> Area -> Sprite
rectangle' ft c a = Rectangle a $ Right (ft, c)

textureSprites :: (a -> (Scale, b)) -> (b -> Texture) -> a -> Pos -> Sprite
textureSprites textures f (second f . textures -> (scale, tx@(Texture dim _ _))) pos = Rectangle (pos, scale |*| dim) (Left tx)

data Pos = Pos {x :: Double, y :: Double} deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

data Scale = Scale {sx :: Double, sy :: Double} deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

data Dimensions = Dimensions {width :: Double, height :: Double} deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

type Area = (Pos, Dimensions)

(|*|) :: Scale -> Dimensions -> Dimensions
(|*|) Scale {sx, sy} Dimensions {width, height} = Dimensions {width = width * sx, height = height * sy}

(|+|) :: Pos -> Dimensions -> Pos
(|+|) Pos {x, y} Dimensions {width, height} = Pos {x = x + width, y = y + height}

(|/|) :: Dimensions -> Dimensions -> Scale
(|/|) Dimensions {width = w1, height = h1} Dimensions {width = w2, height = h2} = Scale {sx = w1 `divideDouble` w2, sy = h1 `divideDouble` h2}

(|-|) :: Pos -> Pos -> Dimensions
(|-|) Pos {x = x1, y = y1} Pos {x = x2, y = y2} = Dimensions {width = x1 - x2, height = y1 - y2}

isWithin :: Pos -> Area -> Bool
isWithin (Pos cx cy) (Pos x y, Dimensions width height) = x <= cx && y <= cy && cx <= (x + width) && cy <= (y + height)

collidesWith :: Area -> Area -> Bool
collidesWith (a1, da) (b1, db) =
  let a2 = a1 |+| da; b2 = b1 |+| db
   in x a1 < x b2 && x a2 > x b1 && y a1 < y b2 && y a2 > y b1

cornerScales :: Corners Scale
cornerScales = Corners (Scale 0 0) (Scale 0 1) (Scale 1 1) (Scale 1 0)

data Corners a = Corners {nw :: a, sw :: a, se :: a, ne :: a} deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON, Functor) -- ne aka north east = upper left corner, etc

instance Foldable Corners where foldr f b (Corners ne se sw nw) = foldr f b [ne, se, sw, nw]

corners :: Area -> Corners Pos
corners (pos, dim) = cornerScales <&> \scale -> pos |+| (scale |*| dim)

instance Num Scale where
  (Scale lx ly) + (Scale rx ry) = Scale (lx + rx) (ly + ry)
  (Scale lx ly) - (Scale rx ry) = Scale (lx - rx) (ly - ry)
  (Scale lx ly) * (Scale rx ry) = Scale (lx * rx) (ly * ry)
  negate (Scale x y) = Scale (- x) (- y)
  abs (Scale x y) = Scale (abs x) (abs y)
  signum (Scale x y) = Scale (signum x) (signum y)
  fromInteger i = Scale (fromInteger i) (fromInteger i)

instance Fractional Scale where
  Scale a b / Scale a' b' = Scale (a / a') (b / b')
  recip (Scale a b) = Scale (recip a) (recip b)
  fromRational r = Scale (fromRational r) (fromRational r)

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
