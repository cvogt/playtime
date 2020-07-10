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
         Rectangle (Border 3) 90 24 $ RGBA 255 0 0 255,
         Rectangle Solid (Pos 90 114) 24 $ RGBA 255 0 0 255
       ]
    <> inventory
  where
    tiles = (groupWith snd $ Map.toList $ unBoard gsBoard) <&> \ne@((_, t) :| _) -> TexturePlacements t 1 $ fst <$> ne
    inventory =
      translate (Pos 200 100)
        <$> [ TexturePlacements Inventory 1 $ pure 0,
              TexturePlacements RedResource 1 $ pure 18,
              TexturePlacements MainCharacter 1 $ pure 3
            ]

translate :: Pos -> TexturePlacements -> TexturePlacements
translate (Pos xd yd) (TexturePlacements t s poss) = TexturePlacements t s $ poss <&> \(Pos x y) -> Pos (x + xd) (y + yd)
translate (Pos xd yd) (Rectangle mode (Pos x y) s c) = Rectangle mode (Pos (x + xd) (y + yd)) s c
