module Platformer.Graphics where

import My.Prelude
import Platformer.GameState
import Playtime.Textures
import Playtime.Types

computeSpritePlacements :: (TextureId -> Texture) -> (EngineState, GameState) -> (Dimensions, [TexturePlacements])
computeSpritePlacements textures (EngineState {..}, GameState {..}) =
  (gsLogicalDimensions, sprites)
  where
    sprites = bullets <> [texturePlacements Plane 1 gsMainCharacterPosition, texturePlacements TopWall 1 gsEnemyPosition]
    texturePlacements :: TextureId -> Scale -> Pos -> TexturePlacements
    texturePlacements textureId scale pos =
      let Texture dim _ _ = textures textureId in TexturePlacements textureId $ Area (pos) $ scale |*| dim
    bullets = toList gsBullets <&> \pos -> texturePlacements TopWall (Scale 0.25 0.25) pos
