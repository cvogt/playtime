module Graphics where

import qualified Data.Map as Map
import My.Prelude
import SpaceMiner.Textures
import SpaceMiner.Types

vizualizeGame :: (TextureId -> Texture) -> GameState -> [Visualization]
vizualizeGame textures GameState {..} =
  tiles
    <> [ TexturePlacements (textures MainCharacter) 1 1 $ pure $ gsMainCharacterPosition,
         TexturePlacements (textures MainCharacter) 2 2 $ pure $ Pos 0 0,
         TexturePlacements (textures MainCharacter) 2 2 $ pure $ Pos 50 50
       ]
  where
    tiles =
      (groupWith snd $ Map.toList $ gsBoard) <&> \ne@((_, t) :| _) -> TexturePlacements (textures t) 1 1 $ fst <$> ne
