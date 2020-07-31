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
textureSprites textures f (second f . textures -> (scale, tx@(Texture dim _ _))) pos = Rectangle (scale * dim, pos) (Left tx)

isWithin :: Pos -> Area -> Bool
isWithin (cx, cy) ((width, height), (x, y)) = x <= cx && y <= cy && cx <= (x + width) && cy <= (y + height)

collidesWith :: Area -> Area -> Bool
collidesWith (da, a1) (db, b1) =
  let a2 = a1 + da; b2 = b1 + db
   in fst a1 < fst b2 && fst a2 > fst b1 && snd a1 < snd b2 && snd a2 > snd b1

cornerScales :: Corners Scale
cornerScales = Corners (0, 0) (0, 1) (1, 1) (1, 0)

data Corners a = Corners {nw :: a, sw :: a, se :: a, ne :: a} deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON, Functor) -- ne aka north east = upper left corner, etc

instance Foldable Corners where foldr f b (Corners ne se sw nw) = foldr f b [ne, se, sw, nw]

corners :: Area -> Corners Pos
corners (dim, pos) = cornerScales <&> \scale -> pos + scale * dim

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

type Pos = (Double, Double)

type Dim = (Double, Double)

type Scale = (Double, Double)

type Area = (Dim, Pos)
