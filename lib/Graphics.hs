module Graphics where

import Codec.Picture.Types (PixelRGBA8 (PixelRGBA8), pixelAt)
import qualified Data.Map as Map
import GHC.Float (double2Int)
import My.Prelude
import SpaceMiner.Textures
import SpaceMiner.Types

computeSpritePlacements :: (Has gs GenericGameState, Has gs PersistentGameState) => (TextureId -> Texture) -> gs -> (Dimensions, [TexturePlacements])
computeSpritePlacements textures gs =
  (gsLogicalDimensions, sprites <> highlightMouserOver)
  where
    GenericGameState {..} = get gs
    PersistentGameState {..} = get gs
    sprites =
      floor <> room
        <> [ TexturePlacements MainCharacter 1 gsMainCharacterPosition,
             TexturePlacements MainCharacter 2 0,
             TexturePlacements MainCharacter 2 50,
             Rectangle (Border 3) 90 24 $ RGBA 255 0 0 255,
             Rectangle Solid (Pos 90 114) 24 $ RGBA 255 0 0 255
           ]
        <> inventory
    Pos cx cy = gsCursorPos
    highlightMouserOver = case findMouseOver of
      Nothing -> []
      Just (TexturePlacements textureId scale pos) ->
        let Texture dim _ _ = textures textureId in [Rectangle (Border 3) (pos -2) ((scale |*| dim) + 4) $ RGBA 0 255 0 255]
      Just (Rectangle Solid pos dim _) -> [Rectangle (Border 3) (pos -2) (dim + 4) $ RGBA 0 255 0 255]
      Just Rectangle {} -> []
    findMouseOver =
      flip find (reverse sprites) $ \case
        TexturePlacements textureId scale pos ->
          let Texture dim _ img = textures textureId
              Pos {x, y} = pos
              Scale {sx, sy} = scale
              px = (cx - x) `divideDouble` sx
              py = (cy - y) `divideDouble` sy
              transparentPixel = case pixelAt img (double2Int px) (double2Int py) of PixelRGBA8 _ _ _ a -> a == 0
           in isMouseOver (scale |*| dim) pos && not transparentPixel
        Rectangle _ pos dim _ -> isMouseOver dim pos
    isMouseOver :: Dimensions -> Pos -> Bool
    isMouseOver (Dimensions width height) (Pos x y) = x < cx && y < cy && (x + width) > cx && (y + height) > cy
    floor = (Map.toList $ unBoard gsFloor) <&> \(pos, t) -> TexturePlacements t 1 pos
    room = (Map.toList $ unBoard gsRoom) <&> \(pos, t) -> TexturePlacements t 1 pos
    -- backup of grouping logic as reminder if needed: (groupWith snd $ Map.toList $ unBoard gsFloor) <&> \ne@((_, t) :| _) ->
    inventory =
      translate (Pos 200 100)
        <$> [ TexturePlacements Inventory 1 0,
              TexturePlacements RedResource 1 18,
              TexturePlacements MainCharacter 1 3
            ]

translate :: Pos -> TexturePlacements -> TexturePlacements
translate (Pos xd yd) (TexturePlacements t s (Pos x y)) = TexturePlacements t s $ Pos (x + xd) (y + yd)
translate (Pos xd yd) (Rectangle mode (Pos x y) s c) = Rectangle mode (Pos (x + xd) (y + yd)) s c
