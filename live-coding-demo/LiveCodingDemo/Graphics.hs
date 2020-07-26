module LiveCodingDemo.Graphics where

import LiveCodingDemo.GameState
import My.Prelude
import Playtime

computeSpritePlacements :: (TextureId -> Texture) -> EngineState -> GameState -> (Dimensions, [TexturePlacements])
computeSpritePlacements textures EngineState {..} GameState {..} =
  (gsLogicalDimensions, sprites)
  where
    sprites =
      ((\pos -> Rectangle Solid (Area pos $ Dimensions 3 3) $ RGBA 255 255 255 255) <$> gsStars)
        <> (texturePlacements EnemyRed (Scale 0.1 0.1) <$> gsEnemies)
        <> (texturePlacements HaskellLoveLogo (Scale 0.1 0.1) <$> gsBullets)
        <> [texturePlacements Plane 1 gsPlayer]
    texturePlacements :: TextureId -> Scale -> Pos -> TexturePlacements
    texturePlacements textureId scale pos =
      let Texture dim _ _ = textures textureId in TexturePlacements textureId $ Area (pos) $ scale |*| dim
