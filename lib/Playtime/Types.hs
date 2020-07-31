{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Playtime.Types
  ( module Playtime.Types,
    Key (..),
    KeyState (..),
    MouseButton (..),
    MouseButtonState (..),
  )
where

import Codec.Picture.Types (Image, PixelRGBA8)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Float
import GHC.Num
import GHC.Real
import qualified Graphics.Rendering.OpenGL.GL as GL (TextureObject)
import "GLFW-b" Graphics.UI.GLFW (Key (..), KeyState (..), MouseButton (..), MouseButtonState (..))
import My.Prelude

-- Event Types
data Color = RGBA Int Int Int Int deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

data Event
  = RenderEvent SystemTime
  | MouseEvent MouseButton MouseButtonState
  | KeyEvent Key KeyState
  | CursorPosEvent Pos
  | WindowSizeEvent Int Int
  | WindowCloseEvent
  deriving (Show)

-- Game State Types

data OneTimeEffect' = Load | Save | Reset deriving (Eq, Ord, Show, Generic, NFData)

data MovementAction' = Up | Down | Left' | Right' deriving (Eq, Ord, Show, Generic, NFData)

data Action = OneTimeEffect OneTimeEffect' | Exit | MovementAction MovementAction' deriving (Eq, Ord, Show, Generic, NFData)

oneTimeEffectMay :: Action -> Maybe OneTimeEffect'
oneTimeEffectMay (OneTimeEffect v) = Just v
oneTimeEffectMay _ = Nothing

movementAction :: Action -> Maybe MovementAction'
movementAction (MovementAction v) = Just v
movementAction _ = Nothing

-- Textures Types
data Texture = Texture
  { tDimensions :: Dim,
    tGLObject :: GL.TextureObject,
    tImage :: Image PixelRGBA8
  }

data FillType = Solid | Border Float

data Sprite = Rectangle Area (Either Texture (FillType, Color))

spriteArea :: Sprite -> Area
spriteArea (Rectangle area _) = area

rectangle :: FillType -> Color -> Dim -> Pos -> Sprite
rectangle ft c d p = Rectangle (d, p) $ Right (ft, c)

rectangle' :: FillType -> Color -> Area -> Sprite
rectangle' ft c a = Rectangle a $ Right (ft, c)

textureSprites :: (a -> (Scale, b)) -> (b -> Texture) -> a -> Pos -> Sprite
textureSprites textures f (second f . textures -> (scale, tx@(Texture dim _ _))) pos = Rectangle (scale *| dim, pos) (Left tx)

isWithin :: Pos -> Area -> Bool
isWithin (cx, cy) ((width, height), (x, y)) = x <= cx && y <= cy && cx <= (x |+ width) && cy <= (y |+ height)

collidesWith :: Area -> Area -> Bool
collidesWith (da, a1) (db, b1) =
  let a2 = a1 |+ da; b2 = b1 |+ db
   in fst a1 < fst b2 && fst a2 > fst b1 && snd a1 < snd b2 && snd a2 > snd b1

cornerScales :: Corners Scale
cornerScales = Corners (0, 0) (0, 1) (1, 1) (1, 0)

data Corners a = Corners {nw :: a, sw :: a, se :: a, ne :: a} deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON, Functor) -- ne aka north east = upper left corner, etc

instance Foldable Corners where foldr f b (Corners ne se sw nw) = foldr f b [ne, se, sw, nw]

corners :: Area -> Corners Pos
corners (dim, pos) = cornerScales <&> \scale -> pos |+ scale *| dim

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

originPos :: Pos
originPos = (0, 0)

data X

data Y

newtype Absolute a = Absolute {unAbsolute :: Double} deriving (Eq, Ord, Show)
  deriving newtype (Num, Fractional, NFData, FromJSON, ToJSON)

newtype Relative a = Relative {unRelative :: Double} deriving (Eq, Ord, Show)
  deriving newtype (Num, Fractional, NFData, FromJSON, ToJSON)

newtype Factor a = Factor {unFactor :: Double} deriving (Eq, Ord, Show)
  deriving newtype (Num, Fractional, NFData, FromJSON, ToJSON)

type Pos = (Absolute X, Absolute Y)

type Dim = (Relative X, Relative Y)

type Scale = (Factor X, Factor Y)

type Area = (Dim, Pos)

instance (Num a, Num b) => Num (a, b) where
  (lx, ly) + (rx, ry) = (lx + rx, ly + ry)
  (lx, ly) - (rx, ry) = (lx - rx, ly - ry)
  (lx, ly) * (rx, ry) = (lx * rx, ly * ry)
  negate (x, y) = (- x, - y)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger i = (fromInteger i, fromInteger i)

instance (Fractional a, Fractional b) => Fractional (a, b) where
  (a, b) / (a', b') = (a / a', b / b')
  recip (a, b) = (recip a, recip b)
  fromRational r = (fromRational r, fromRational r)

xRelative :: Double -> Relative X
xRelative = Relative

yRelative :: Double -> Relative Y
yRelative = Relative

xAbsolute :: Double -> Absolute X
xAbsolute = Absolute

yAbsolute :: Double -> Absolute Y
yAbsolute = Absolute

xFactor :: Double -> Factor X
xFactor = Factor

yFactor :: Double -> Factor Y
yFactor = Factor

class AdditionPairWise a b where (|+) :: a -> b -> a

class SubtractionPairWise a b where (|-|) :: a -> a -> b

class SubtractionPairWiseLeft a b where (|-) :: a -> b -> a

class MultiplicationPairWise a b where (|*|) :: a -> b -> a

class MultiplicationPairWiseRight a b where (*|) :: a -> b -> b

class DivisionPairWise a b where (|/|) :: a -> a -> b

class DivisionPairWiseLeft a b where (|/) :: a -> b -> a

class ModuloPairWise a b where (|%) :: a -> b -> a

class Modulo'PairWise a b where (|%%|) :: a -> b -> a

infixl 7 |*|, *|, |/|, |%, |%%|

infixl 6 |+, |-|, |-

instance AdditionPairWise (Absolute a) (Relative a) where
  (Absolute l) |+ (Relative r) = Absolute $ l + r

instance AdditionPairWise (Relative a) (Relative a) where (Relative l) |+ (Relative r) = Relative $ l + r

instance AdditionPairWise Pos (Relative X) where (a, a') |+ r = (a |+ r, a')

instance AdditionPairWise Pos (Relative Y) where (a, a') |+ r = (a, a' |+ r)

instance AdditionPairWise Dim Dim where (|+) = pairWise (|+) (|+)

instance AdditionPairWise Pos Dim where (|+) = pairWise (|+) (|+)

instance AdditionPairWise (Absolute X) Dim where l |+ dim = l |+ fst dim

instance AdditionPairWise (Absolute Y) Dim where l |+ dim = l |+ snd dim

instance AdditionPairWise (Relative X) Dim where l |+ dim = l |+ fst dim

instance AdditionPairWise (Relative Y) Dim where l |+ dim = l |+ snd dim

instance SubtractionPairWise (Absolute a) (Relative a) where (Absolute l) |-| (Absolute r) = Relative $ l - r

instance SubtractionPairWise Pos Dim where (|-|) = pairWise (|-|) (|-|)

instance SubtractionPairWiseLeft (Absolute a) (Relative a) where (Absolute l) |- (Relative r) = Absolute $ l - r

instance SubtractionPairWiseLeft (Relative a) (Relative a) where (Relative l) |- (Relative r) = Relative $ l - r

instance SubtractionPairWiseLeft (Absolute X) Dim where l |- dim = l |- fst dim

instance SubtractionPairWiseLeft (Absolute Y) Dim where l |- dim = l |- snd dim

instance SubtractionPairWiseLeft (Relative X) Dim where l |- dim = l |- fst dim

instance SubtractionPairWiseLeft (Relative Y) Dim where l |- dim = l |- snd dim

instance SubtractionPairWiseLeft Pos (Relative X) where pos |- r = (fst pos |- r, snd pos)

instance SubtractionPairWiseLeft Pos (Relative Y) where pos |- r = (fst pos, snd pos |- r)

instance SubtractionPairWiseLeft Pos Dim where (|-) = pairWise (|-) (|-)

instance MultiplicationPairWiseRight (Factor a) (Relative a) where (Factor l) *| (Relative r) = Relative $ l * r

instance MultiplicationPairWise (Relative a) (Factor a) where (Relative r) |*| (Factor l) = Relative $ l * r

instance MultiplicationPairWise (Factor a) (Factor a) where (Factor l) |*| (Factor r) = Factor $ l * r

instance MultiplicationPairWise (Factor X) Scale where l |*| scale = l |*| fst scale

instance MultiplicationPairWise (Factor Y) Scale where l |*| scale = l |*| snd scale

instance MultiplicationPairWise (Relative X) Scale where l |*| scale = l |*| fst scale

instance MultiplicationPairWise (Relative Y) Scale where l |*| scale = l |*| snd scale

instance MultiplicationPairWiseRight Scale (Relative X) where scale *| r = fst scale *| r

instance MultiplicationPairWiseRight Scale (Relative Y) where scale *| r = snd scale *| r

instance MultiplicationPairWiseRight (Factor X) Dim where scale *| r = (scale *| fst r, snd r)

instance MultiplicationPairWiseRight (Factor Y) Dim where scale *| r = (fst r, scale *| snd r)

instance MultiplicationPairWise Scale Scale where (|*|) = pairWise (|*|) (|*|)

instance MultiplicationPairWise Dim Scale where (|*|) = pairWise (|*|) (|*|)

instance MultiplicationPairWiseRight Scale Dim where (*|) = pairWise (*|) (*|)

instance DivisionPairWise (Relative a) (Factor a) where (Relative l) |/| (Relative r) = Factor $ l / r

instance DivisionPairWise Dim Scale where (|/|) = pairWise (|/|) (|/|)

instance DivisionPairWiseLeft (Relative a) (Factor a) where (Relative l) |/ (Factor r) = Relative $ l / r

instance DivisionPairWiseLeft Dim Scale where (|/) = pairWise (|/) (|/)

instance ModuloPairWise (Absolute a) (Relative a) where
  (Absolute l) |% (Relative r) = Absolute $ int2Double $ double2Int l `mod` double2Int r

instance ModuloPairWise (Relative X) Dim where
  (Relative l) |% r = Relative $ int2Double $ double2Int l `mod` double2Int (unRelative $ fst r)

instance ModuloPairWise (Relative Y) Dim where
  (Relative l) |% r = Relative $ int2Double $ double2Int l `mod` double2Int (unRelative $ snd r)

instance ModuloPairWise Dim (Relative X) where
  l |% (Relative r) = (x, snd l)
    where
      x = Relative $ int2Double $ double2Int (unRelative $ fst l) `mod` double2Int r

instance ModuloPairWise Dim (Relative Y) where
  l |% (Relative r) = (fst l, y)
    where
      y = Relative $ int2Double $ double2Int (unRelative $ snd l) `mod` double2Int r

instance ModuloPairWise Pos (Relative X) where
  l |% (Relative r) = (x, snd l)
    where
      x = Absolute $ int2Double $ double2Int (unAbsolute $ fst l) `mod` double2Int r

instance ModuloPairWise Pos (Relative Y) where
  l |% (Relative r) = (fst l, y)
    where
      y = Absolute $ int2Double $ double2Int (unAbsolute $ snd l) `mod` double2Int r

instance ModuloPairWise Pos Dim where (|%) = pairWise (|%) (|%)

instance ModuloPairWise Dim Dim where (|%) = pairWise (|%) (|%)

instance ModuloPairWise (Relative a) (Relative a) where
  (Relative l) |% (Relative r) = Relative $ int2Double $ double2Int l `mod` double2Int r

instance Modulo'PairWise (Absolute a) (Relative a) where (Absolute l) |%%| (Relative r) = Absolute $ l `mod'` r

instance Modulo'PairWise (Relative a) (Relative a) where (Relative l) |%%| (Relative r) = Relative $ l `mod'` r

--FIXME replace mod' with mod + (v - floor v)
instance Modulo'PairWise Pos (Relative X) where
  l |%%| (Relative r) = (x, snd l)
    where
      x = Absolute $ (unAbsolute $ fst l) `mod'` r

instance Modulo'PairWise Pos (Relative Y) where
  l |%%| (Relative r) = (fst l, y)
    where
      y = Absolute $ (unAbsolute $ snd l) `mod'` r

instance Modulo'PairWise Pos Dim where (|%%|) = pairWise (|%%|) (|%%|)

instance Modulo'PairWise Dim Dim where (|%%|) = pairWise (|%%|) (|%%|)

pairWise :: (a -> b -> c) -> (a' -> b' -> c') -> (a, a') -> (b, b') -> (c, c')
pairWise f g (a, a') (b, b') = (f a b, g a' b')
