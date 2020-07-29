module LiveCodingDemo.Visualize where

import LiveCodingDemo.GameState
import My.Prelude
import Playtime

visualize :: (TextureId -> Texture) -> EngineState -> GameState -> [TexturePlacements TextureId]
visualize textures EngineState {..} GameState {..} =
  let sprite :: TextureUse -> Pos -> TexturePlacements TextureId
      sprite tu pos = TexturePlacements (tuId tu) $ textureArea textures tu pos
   in [sprite plane gsPlayer]
        <> (sprite bullet <$> gsBullets)
        <> (sprite enemy <$> gsEnemies)
        <> ((\pos -> Rectangle Solid (Area pos $ Dimensions 3 3) $ RGBA 180 180 180 255) <$> gsStars)

all_textures :: [TextureId]
all_textures = tuId <$> [plane, enemy, bullet]
