module Playtime.Texture where

import Codec.Picture.Types (Image, PixelRGBA8 (PixelRGBA8), pixelAt)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Float
import GHC.Num
import qualified Graphics.Rendering.OpenGL.GL as GL (TextureObject)
import My.Prelude
import Playtime.Geometry

-- Textures Types
data Texture = Texture
  { tDimensions :: Dim,
    tGLObject :: GL.TextureObject,
    tImage :: Image PixelRGBA8
  }

data FillType = Solid | Border Float

data Sprite = Rectangle Area (Either Texture (FillType, Color))

data Color = RGBA Int Int Int Int deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

spriteArea :: Sprite -> Area
spriteArea (Rectangle area _) = area

rectangle :: FillType -> Color -> Dim -> Pos -> Sprite
rectangle ft c d p = Rectangle (d, p) $ Right (ft, c)

rectangle' :: FillType -> Color -> Area -> Sprite
rectangle' ft c a = Rectangle a $ Right (ft, c)

textureSprites :: (a -> (Scale, b)) -> (b -> Texture) -> a -> Pos -> Sprite
textureSprites textures f (second f . textures -> (scale, tx@(Texture dim _ _))) pos = Rectangle (scale * dim, pos) (Left tx)

textureDim :: (a -> (Scale, b)) -> (b -> Texture) -> a -> Dim
textureDim textureScale textures i =
  let (scale, b) = textureScale i
      Texture dim _ _ = textures b
   in scale * dim

translate :: Dim -> Sprite -> Sprite
translate offset (Rectangle (dim, pos) v) = Rectangle (dim, pos + offset) v

isPixelTransparent :: Pos -> Sprite -> Bool
isPixelTransparent esCursorPos = \case
  Rectangle area@(dim', pos) (Left (Texture dim _ img)) ->
    let p = (esCursorPos - pos) * dim / dim'
     in esCursorPos `isWithin` area && not (isPixelTransparent' img p)
  Rectangle area _ -> esCursorPos `isWithin` area

isPixelTransparent' :: Image PixelRGBA8 -> (Double, Double) -> Bool
isPixelTransparent' img p = case pixelAt img (double2Int $ fst p) (double2Int $ snd p) of PixelRGBA8 _ _ _ a -> a == 0
