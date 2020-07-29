module ShootEmUp.Visualize where

import My.Prelude
import Playtime
import ShootEmUp.GameState

visualize :: (TextureId -> Texture) -> EngineState -> GameState -> [TexturePlacements TextureId]
visualize (textureArea textureUse -> area) EngineState {..} GameState {..} =
  [sprite area Plane gsMainCharacter]
    <> (sprite area Heart <$> gsBullets)
    <> (sprite area Enemy <$> gsEnemies)
    <> stars
    <> showDragAndDrop gsDragAndDrop (sprite area Heart)
    <> showDragAndDrop gsDragAndDrop (\pos -> Rectangle (Border 3) (area Heart pos) $ RGBA 255 0 0 255)
  where
    stars =
      gsStars <&> \((+ 1) -> size, pos) ->
        Rectangle Solid (Area pos $ Dimensions (- size) size) $ -- -size for x so stars extend to the left and can leave the screen smoothly
          RGBA 180 180 180 255
