module ShootEmUp.Visualize where

import My.Prelude
import Playtime
import ShootEmUp.GameState

visualize :: (TextureId -> Texture) -> EngineState -> GameState -> (Dimensions, [TexturePlacements])
visualize textures EngineState {..} GameState {..} =
  (esLogicalDimensions, sprites)
  where
    sprites = stars <> bullets <> enemies <> [texturePlacements Plane 1 gsMainCharacterPosition]
    texturePlacements :: TextureId -> Scale -> Pos -> TexturePlacements
    texturePlacements textureId scale pos =
      let Texture dim _ _ = textures textureId in TexturePlacements textureId $ Area (pos) $ scale |*| dim
    bullets = texturePlacements HaskellLoveLogo (Scale 0.025 0.025) <$> toList gsBullets
    enemies = texturePlacements EnemyRed (Scale 0.1 0.1) <$> toList gsEnemies
    stars =
      gsStars <&> \((+ 1) -> size, pos) ->
        Rectangle Solid (Area pos $ Dimensions (- size) size) $ -- -size for x so stars extend to the left and can leave the screen smoothly
          RGBA 180 180 180 255
