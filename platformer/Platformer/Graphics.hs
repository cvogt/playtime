module Platformer.Graphics where

import qualified Data.Map as Map
import My.Prelude
import Platformer.GameState
import Playtime

computeSpritePlacements :: (TextureId -> Texture) -> EngineState -> GameState -> (Dimensions, [TexturePlacements])
computeSpritePlacements textures EngineState {..} GameState {..} =
  (esLogicalDimensions, sprites)
  where
    sprites = room <> [texturePlacements MainCharacter 1 gsMainCharacterPosition]
    texturePlacements :: TextureId -> Scale -> Pos -> TexturePlacements
    texturePlacements textureId scale pos =
      let Texture dim _ _ = textures textureId in TexturePlacements textureId $ Area (pos) $ scale |*| dim
    room = (Map.toList $ unBoard gsRoom) <&> \(pos, t) -> texturePlacements t 1 pos

-- backup of grouping logic as reminder if needed: (groupWith snd $ Map.toList $ unBoard gsFloor) <&> \ne@((_, t) :| _) ->
