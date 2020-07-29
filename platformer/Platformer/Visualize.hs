module Platformer.Visualize where

import qualified Data.Map as Map
import My.Prelude
import Platformer.GameState
import Playtime

visualize :: (TextureId -> Texture) -> EngineState -> GameState -> [TexturePlacements TextureId]
visualize (textureArea textureUse -> area) EngineState {..} GameState {..} =
  [sprite area MainCharacter gsMainCharacter] <> room
  where
    room = (Map.toList $ unBoard gsRoom) <&> \(pos, t) -> sprite area t pos

-- backup of grouping logic as reminder if needed: (groupWith snd $ Map.toList $ unBoard gsFloor) <&> \ne@((_, t) :| _) ->
