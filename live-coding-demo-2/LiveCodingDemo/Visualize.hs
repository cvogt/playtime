{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module LiveCodingDemo.Visualize where

import LiveCodingDemo.GameState
import My.Prelude
import Playtime

visualize :: (TextureId -> Pos -> Sprite) -> EngineState -> GameState -> [Sprite]
visualize sprite EngineState {..} GameState {..} =
  let
   in [sprite Plane gsPlayer]
        <> (sprite Heart <$> gsHearts)
        <> (sprite Enemy <$> gsEnemies)
        <> (rectangle' Solid (RGBA 180 180 180 255) . first dupe <$> gsStars)
