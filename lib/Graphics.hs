module Graphics where

import qualified Data.Map as Map
import My.Prelude
import SpaceMiner.Textures
import SpaceMiner.Types

computeSpritePlacements :: GameState -> [TexturePlacements]
computeSpritePlacements GameState {..} =
  tiles
    <> [ TexturePlacements MainCharacter 1 1 $ pure $ gsMainCharacterPosition,
         TexturePlacements MainCharacter 2 2 $ pure $ Pos 0 0,
         TexturePlacements MainCharacter 2 2 $ pure $ Pos 50 50
       ]
  where
    tiles =
      (groupWith snd $ Map.toList $ gsBoard) <&> \ne@((_, t) :| _) -> TexturePlacements t 1 1 $ fst <$> ne
