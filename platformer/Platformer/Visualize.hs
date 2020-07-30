module Platformer.Visualize where

import qualified Data.Map as Map
import My.Prelude
import Platformer.GameState
import Playtime

visualize :: (FilePath -> Texture) -> EngineState -> GameState -> [Sprite]
visualize (textureSprite textures -> sprite) EngineState {..} GameState {..} =
  [sprite MainCharacter gsMainCharacter] <> room
  where
    room = (Map.toList $ unBoard gsRoom) <&> \(pos, t) -> sprite t pos

-- backup of grouping logic as reminder if needed: (groupWith snd $ Map.toList $ unBoard gsFloor) <&> \ne@((_, t) :| _) ->
