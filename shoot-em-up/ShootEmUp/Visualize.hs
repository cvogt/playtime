module ShootEmUp.Visualize where

import My.Prelude
import Playtime
import ShootEmUp.GameState

visualize :: (FilePath -> Texture) -> EngineState -> GameState -> [Sprite]
visualize loadedTextures EngineState {..} GameState {..} =
  let sprite = textureSprite textures loadedTextures
      area = textureArea textures loadedTextures
   in [sprite Plane gsMainCharacter]
        <> (sprite Heart <$> gsBullets)
        <> (sprite Enemy <$> gsEnemies)
        <> stars
        <> showDragAndDrop gsDragAndDrop (sprite Heart)
        <> showDragAndDrop gsDragAndDrop (\pos -> uncurry (rectangle (Border 3) (RGBA 255 0 0 255)) $ swap $ area Heart pos)
  where
    stars =
      gsStars <&> \((+ 1) -> size, pos) ->
        rectangle Solid (RGBA 180 180 180 255) (Dimensions (- size) size) pos -- -size for x so stars extend to the left and can leave the screen smoothly
