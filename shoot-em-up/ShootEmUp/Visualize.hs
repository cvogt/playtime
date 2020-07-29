module ShootEmUp.Visualize where

import My.Prelude
import Playtime
import ShootEmUp.GameState

visualize :: (TextureId -> Texture) -> EngineState -> GameState -> [TexturePlacements TextureId]
visualize (textureArea -> area) EngineState {..} GameState {..} =
  let sprite :: TextureUse -> Pos -> TexturePlacements TextureId
      sprite tu pos = TexturePlacements (tuId tu) $ area tu pos
   in [sprite plane gsMainCharacter]
        <> (sprite bullet <$> gsBullets)
        <> (sprite enemy <$> gsEnemies)
        <> stars
        <> showDragAndDrop gsDragAndDrop (sprite bullet)
        <> showDragAndDrop gsDragAndDrop (\pos -> Rectangle (Border 3) (area bullet pos) $ RGBA 255 0 0 255)
  where
    stars =
      gsStars <&> \((+ 1) -> size, pos) ->
        Rectangle Solid (Area pos $ Dimensions (- size) size) $ -- -size for x so stars extend to the left and can leave the screen smoothly
          RGBA 180 180 180 255
