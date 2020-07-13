module SpaceMiner.Graphics where

import Codec.Picture.Types (PixelRGBA8 (PixelRGBA8), pixelAt)
import qualified Data.Map as Map
import GHC.Float (double2Int)
import My.Prelude
import Playtime.Textures
import Playtime.Types
import Playtime.Util
import SpaceMiner.GameState

computeSpritePlacements :: (TextureId -> Texture) -> (EngineState, GameState) -> (Dimensions, [TexturePlacements])
computeSpritePlacements textures (EngineState {..}, GameState {..}) =
  (gsLogicalDimensions, sprites <> highlightMouserOver)
  where
    sprites =
      floor <> room
        <> [ texturePlacements MainCharacter 1 gsMainCharacterPosition,
             texturePlacements MainCharacter 2 0,
             texturePlacements MainCharacter 2 50,
             rectangle (Border 3) 90 24 $ RGBA 255 0 0 255,
             rectangle Solid (Pos 90 114) 24 $ RGBA 255 255 0 255
           ]
        <> inventory
    texturePlacements :: TextureId -> Scale -> Pos -> TexturePlacements
    texturePlacements textureId scale pos =
      let Texture dim _ _ = textures textureId in TexturePlacements textureId $ Area (pos) $ scale |*| dim
    rectangle :: FillType -> Pos -> Dimensions -> Color -> TexturePlacements
    rectangle fillType pos dimensions color = Rectangle fillType (Area pos dimensions) color
    highlightMouserOver = case findMouseOver of
      Nothing -> []
      Just (TexturePlacements _ area) -> highlight area
      Just (Rectangle Solid area _) -> highlight area
      Just Rectangle {} -> []
    highlight (Area pos dim) = [Rectangle (Border 3) (Area (pos -2) (dim + 4)) $ RGBA 0 255 0 255]
    findMouseOver =
      flip find (reverse sprites) $ \case
        TexturePlacements textureId area@(Area pos dim') ->
          let Texture dim _ img = textures textureId
              Pos cx cy = gsCursorPos
              Pos {x, y} = pos
              Scale {sx, sy} = dim' |/| dim
              px = (cx - x) `divideDouble` sx
              py = (cy - y) `divideDouble` sy
              transparentPixel = case pixelAt img (double2Int px) (double2Int py) of PixelRGBA8 _ _ _ a -> a == 0
           in gsCursorPos `isWithin` area && not transparentPixel
        Rectangle _ area _ -> gsCursorPos `isWithin` area
    floor = (Map.toList $ unBoard gsFloor) <&> \(pos, t) -> texturePlacements t 1 pos
    room = (Map.toList $ unBoard gsRoom) <&> \(pos, t) -> texturePlacements t 1 pos
    -- backup of grouping logic as reminder if needed: (groupWith snd $ Map.toList $ unBoard gsFloor) <&> \ne@((_, t) :| _) ->
    inventory =
      translate (Pos 200 100)
        <$> [ texturePlacements Inventory 1 0,
              texturePlacements RedResource 1 18,
              texturePlacements MainCharacter 1 3
            ]
