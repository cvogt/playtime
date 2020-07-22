module ShootEmUp.Graphics where

import My.Prelude
import Playtime.Textures
import Playtime.Types
import ShootEmUp.GameState

computeSpritePlacements :: (TextureId -> Texture) -> (EngineState, GameState) -> (Dimensions, [TexturePlacements])
computeSpritePlacements textures (EngineState {..}, GameState {..}) =
  (gsLogicalDimensions, sprites)
  where
    sprites = bullets <> enemies <> [texturePlacements Plane 1 gsMainCharacterPosition]
    texturePlacements :: TextureId -> Scale -> Pos -> TexturePlacements
    texturePlacements textureId scale pos =
      let Texture dim _ _ = textures textureId in TexturePlacements textureId $ Area (pos) $ scale |*| dim
    bullets = texturePlacements TopWall (Scale 0.25 0.25) <$> toList gsBullets
    enemies = texturePlacements TopWall 1 <$> toList gsEnemies
