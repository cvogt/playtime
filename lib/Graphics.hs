module Graphics where

import My.Prelude
import SpaceMiner.Textures
import SpaceMiner.Types

vizualizeGame :: (TextureId -> Texture) -> GameState -> [Visualization]
vizualizeGame textures gameState =
  [ TexturePlacements (textures FloorPlate) 1 1 $ (uncurry TexturePlacement . unCursorPos <$> (toList $ gsBoard gameState)),
    TexturePlacements (textures MainCharacter) 1 1 $ [uncurry TexturePlacement . unCursorPos $ gsMainCharacterPosition gameState],
    TexturePlacements (textures MainCharacter) 4 4 $ [TexturePlacement 0 0],
    TexturePlacements (textures MainCharacter) 4 4 $ [TexturePlacement 100 100]
  ]
