module Graphics where

import qualified Data.Map as Map
import My.Prelude
import SpaceMiner.Textures
import SpaceMiner.Types

computeSpritePlacements :: GameState -> [TexturePlacements]
computeSpritePlacements (GameState GenericGameState {..} TransientGameState {..} PersistentGameState {..}) =
  tiles
    <> [ TexturePlacements MainCharacter 1 $ pure gsMainCharacterPosition,
         TexturePlacements MainCharacter 2 $ pure 0,
         TexturePlacements MainCharacter 2 $ pure 50,
         TexturePlacements RedResource 1 $ pure 80,
         Rectangle 90 24 $ RGBA 255 0 0 255
       ]
  where
    tiles =
      (groupWith snd $ Map.toList $ unBoard gsBoard) <&> \ne@((_, t) :| _) -> TexturePlacements t 1 $ fst <$> ne
