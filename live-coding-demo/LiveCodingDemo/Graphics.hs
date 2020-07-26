module LiveCodingDemo.Graphics where

import LiveCodingDemo.GameState
import My.Prelude
import Playtime.Textures
import Playtime.Types

computeSpritePlacements :: (TextureId -> Texture) -> EngineState -> GameState -> (Dimensions, [TexturePlacements])
computeSpritePlacements textures EngineState {..} GameState {..} =
  (gsLogicalDimensions, sprites)
  where
    sprites =
      [ texturePlacements Plane 1 (Pos 100 100),
        Rectangle Solid (Area (Pos 10 10) $ Dimensions 50 50) $ RGBA 255 0 0 255
      ]
    texturePlacements :: TextureId -> Scale -> Pos -> TexturePlacements
    texturePlacements textureId scale pos =
      let Texture dim _ _ = textures textureId in TexturePlacements textureId $ Area (pos) $ scale |*| dim
