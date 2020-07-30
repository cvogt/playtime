module LiveCodingDemo.Visualize where

import LiveCodingDemo.GameState
import My.Prelude
import Playtime

visualize :: (TextureId -> Pos -> Sprite) -> EngineState -> GameState -> [Sprite]
visualize sprite EngineState {..} GameState {..} =
  let
   in [sprite Plane gsPlayer]
        <> (sprite Heart <$> gsBullets)
        <> (sprite Enemy <$> gsEnemies)
        <> (rectangle Solid (RGBA 180 180 180 255) (3, 3) <$> gsStars)
