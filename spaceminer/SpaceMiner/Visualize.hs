module SpaceMiner.Visualize where

import Codec.Picture.Types (PixelRGBA8 (PixelRGBA8), pixelAt)
import qualified Data.Map as Map
import GHC.Float (double2Int)
import My.Prelude
import Playtime
import SpaceMiner.GameState

visualize :: (TextureId -> Texture) -> EngineState -> GameState -> [TexturePlacements TextureId]
visualize textures EngineState {..} GameState {..} =
  highlightMouserOver <> sprites
  where
    area = textureArea textureUse textures
    sprites =
      inventoryUI
        <> [ sprite area MainCharacter gsMainCharacter,
             sprite area MainCharacter 0,
             sprite area MainCharacter 50,
             rectangle (Border 3) 90 24 $ RGBA 255 0 0 255,
             rectangle Solid (Pos 90 114) 24 $ RGBA 255 255 0 255
           ]
        <> room
        <> floor
    rectangle :: FillType -> Pos -> Dimensions -> Color -> TexturePlacements TextureId
    rectangle fillType pos dimensions color = Rectangle fillType (Area pos dimensions) color
    highlightMouserOver = case findMouseOver of
      Nothing -> []
      Just (TexturePlacements _ area') -> highlight area'
      Just (Rectangle Solid area' _) -> highlight area'
      Just Rectangle {} -> []
    highlight (Area pos dim) = [Rectangle (Border 3) (Area (pos -2) (dim + 4)) $ RGBA 0 255 0 255]
    findMouseOver =
      flip find (reverse sprites) $ \case
        TexturePlacements textureId area'@(Area pos dim') ->
          let Texture dim _ img = textures textureId
              Pos cx cy = esCursorPos
              Pos {x, y} = pos
              Scale {sx, sy} = dim' |/| dim
              px = (cx - x) `divideDouble` sx
              py = (cy - y) `divideDouble` sy
              transparentPixel = case pixelAt img (double2Int px) (double2Int py) of PixelRGBA8 _ _ _ a -> a == 0
           in esCursorPos `isWithin` area' && not transparentPixel
        Rectangle _ area' _ -> esCursorPos `isWithin` area'
    floor = (Map.toList $ unBoard gsFloor) <&> \(pos, t) -> sprite area t pos
    room = (Map.toList $ unBoard gsRoom) <&> \(pos, t) -> sprite area t pos
    -- backup of grouping logic as reminder if needed: (groupWith snd $ Map.toList $ unBoard gsFloor) <&> \ne@((_, t) :| _) ->
    inventoryUI =
      translate (Pos 200 100)
        <$> [ sprite area Inventory 0,
              sprite area RedResource 18,
              sprite area MainCharacter 3
            ]
