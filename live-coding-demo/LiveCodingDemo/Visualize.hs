module LiveCodingDemo.Visualize where

import LiveCodingDemo.GameState
import My.Prelude
import Playtime

visualize :: (TextureId -> Texture) -> EngineState -> GameState -> [TexturePlacements TextureId]
visualize (textureArea textureUse -> area) EngineState {..} GameState {..} =
  [sprite area Plane gsPlayer]
    <> (sprite area Heart <$> gsBullets)
    <> (sprite area Enemy <$> gsEnemies)
    <> ((\pos -> Rectangle Solid (Area pos $ Dimensions 3 3) $ RGBA 180 180 180 255) <$> gsStars)
