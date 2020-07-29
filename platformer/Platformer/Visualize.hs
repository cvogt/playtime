module Platformer.Visualize where

import qualified Data.Map as Map
import My.Prelude
import Platformer.GameState
import Playtime

visualize :: (TextureId -> Texture) -> EngineState -> GameState -> [TexturePlacements TextureId]
visualize textures EngineState {..} GameState {..} =
  [texturePlacements main_character gsMainCharacter] <> room
  where
    texturePlacements :: TextureUse -> Pos -> TexturePlacements TextureId
    texturePlacements (TextureUse scale textureId) pos =
      let Texture dim _ _ = textures textureId in TexturePlacements textureId $ Area (pos) $ scale |*| dim
    room = (Map.toList $ unBoard gsRoom) <&> \(pos, t) -> texturePlacements t pos

-- backup of grouping logic as reminder if needed: (groupWith snd $ Map.toList $ unBoard gsFloor) <&> \ne@((_, t) :| _) ->
