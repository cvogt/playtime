module SpaceMiner.Visualize where

import qualified Data.Map as Map
import My.Prelude
import Playtime
import SpaceMiner.GameState

visualize :: (TextureId -> Pos -> Sprite) -> EngineState -> GameState -> [Sprite]
visualize sprite EngineState {..} GameState {..} =
  highlightMouserOver <> sprites
  where
    sprites =
      container <> inventoryUI
        <> [ sprite MainCharacter gsMainCharacter,
             sprite MainCharacter 0,
             sprite MainCharacter 50,
             rectangle (Border 3) (RGBA 255 0 0 255) 24 90,
             rectangle Solid (RGBA 255 255 0 255) 24 (90, 114)
           ]
        <> room
        <> floor
    highlightMouserOver = case findMouseOver of
      Nothing -> []
      Just (Rectangle area _) -> highlight area
    highlight (dim, pos) = [rectangle (Border 3) (RGBA 0 255 0 255) (dim + 4) (pos -2)]
    findMouseOver = flip find sprites $ isPixelTransparent esCursorPos
    floor = (Map.toList $ unBoard gsFloor) <&> \(pos, t) -> sprite t pos
    room = (Map.toList $ unBoard gsRoom) <&> \(pos, t) -> sprite t pos
    -- backup of grouping logic as reminder if needed: (groupWith snd $ Map.toList $ unBoard gsFloor) <&> \ne@((_, t) :| _) ->
    inventoryUI =
      let Container pos _spacing _columns _contents = gsInventory
       in translate pos
            <$> [ sprite RedResource 18,
                  sprite MainCharacter 3,
                  sprite Inventory 0
                ]
    container =
      let Container pos _spacing _columns _contents = gsContainer
       in translate pos
            <$> [ sprite RedResource 18,
                  sprite MainCharacter 3,
                  sprite Inventory 0
                ]
