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


instance Num Scale' where
  (lx, ly) + (rx, ry) = (lx + rx, ly + ry)
  (lx, ly) - (rx, ry) = (lx - rx, ly - ry)
  (lx, ly) * (rx, ry) = (lx * rx, ly * ry)
  negate (x, y) = (- x, - y)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger i = (fromInteger i, fromInteger i)

instance Fractional Scale' where
  (a, b) / (a', b') = (a / a', b / b')
  recip (a, b) = (recip a, recip b)
  fromRational r = (fromRational r, fromRational r)

data Pos = Pos {x :: Double, y :: Double} deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

data Scale = Scale {sx :: Double, sy :: Double} deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

data Dimensions = Dimensions {width :: Double, height :: Double} deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

type Area = (Pos, Dimensions)

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
corners (pos, dim) = cornerScales <&> \scale -> pos |+| (scale |*| dim :: Dimensions)

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

{-
The following are type-safe types and functions for coordinate and vector arithmetics.

After several bugs accidentally adding a height to an x coordinate,
this will prevent them going forward.

Coordinate type safety is achieve by distinguishing X and Y coordinates using a Phantom type.
We are also distinguishing absolute and relative coordiantes as well as factor from each other.

This prevents things like adding absolute coordinates to each other, which is non-sensible,
and limiting allowed operatings to sensible one such as adding a relative coordinate to an
absolute one.

Vector arithmetic type-safety distinguishe
-}

instance AdditionElementWise Pos Dimensions Pos where
  (|+|) Pos {x, y} Dimensions {width, height} = Pos {x = x + width, y = y + height}

instance SubtractionElementWise Pos Pos Dimensions where
  (|-|) Pos {x = x1, y = y1} Pos {x = x2, y = y2} = Dimensions {width = x1 - x2, height = y1 - y2}

instance MultiplicationElementWise Scale Dimensions Dimensions where
  (|*|) Scale {sx, sy} Dimensions {width, height} = Dimensions {width = width * sx, height = height * sy}

instance DivisionElementWise Dimensions Dimensions Scale where
  (|/|) Dimensions {width = w1, height = h1} Dimensions {width = w2, height = h2} = Scale {sx = w1 `divideDouble` w2, sy = h1 `divideDouble` h2}

data X

data Y

newtype Absolute a = Absolute Double deriving (Eq, Ord, Show)
  deriving newtype (Num, Fractional, NFData, FromJSON, ToJSON)

newtype Relative a = Relative Double deriving (Eq, Ord, Show)
  deriving newtype (Num, Fractional, NFData, FromJSON, ToJSON)

newtype Factor a = Factor Double deriving (Eq, Ord, Show)
  deriving newtype (Num, Fractional, NFData, FromJSON, ToJSON)

type Vector2 t = (t X, t Y)

type Pos' = Vector2 Absolute

type Dim' = Vector2 Relative

type Scale' = Vector2 Factor

type Rect = (Dim', Pos')

class AdditionElementWise a b c where (|+|) :: a -> b -> c

class SubtractionElementWise a b c where (|-|) :: a -> b -> c

class MultiplicationElementWise a b c where (|*|) :: a -> b -> c

class DivisionElementWise a b c where (|/|) :: a -> b -> c

infixl 7 |*|, |/|

infixl 6 |+|, |-|

instance AdditionElementWise (Absolute a) (Relative a) (Absolute a) where
  (Absolute l) |+| (Relative r) = Absolute $ l * r

instance AdditionElementWise (Relative a) (Absolute a) (Absolute a) where
  (|+|) = flip (|+|)

instance SubtractionElementWise (Absolute a) (Absolute a) (Relative a) where
  (Absolute l) |-| (Absolute r) = Relative $ l * r

instance MultiplicationElementWise (Factor a) (Relative a) (Relative a) where
  (Factor l) |*| (Relative r) = Relative $ l * r

instance MultiplicationElementWise (Relative a) (Factor a) (Relative a) where
  (|*|) = flip (|*|)

instance DivisionElementWise (Relative a) (Relative a) (Factor a) where
  (Relative l) |/| (Relative r) = Factor $ l * r

instance
  (AdditionElementWise a b c, AdditionElementWise a' b' c') =>
  AdditionElementWise (a, a') (b, b') (c, c')
  where
  (a, a') |+| (b, b') = (a |+| b, a' |+| b')

instance
  (SubtractionElementWise a b c, SubtractionElementWise a' b' c') =>
  SubtractionElementWise (a, a') (b, b') (c, c')
  where
  (a, a') |-| (b, b') = (a |-| b, a' |-| b')

instance
  (MultiplicationElementWise a b c, MultiplicationElementWise a' b' c') =>
  MultiplicationElementWise (a, a') (b, b') (c, c')
  where
  (a, a') |*| (b, b') = (a |*| b, a' |*| b')

instance
  (DivisionElementWise a b c, DivisionElementWise a' b' c') =>
  DivisionElementWise (a, a') (b, b') (c, c')
  where
  (a, a') |/| (b, b') = (a |/| b, a' |/| b')
