{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}

module Graphics where

import Bitmap
import Data.FileEmbed
import qualified Data.Set as Set
import GHC.Float (int2Double)
import GHC.Real ((/), round)
import GLFWHelpers
import Game
import Graphics.Rendering.OpenGL.GL (Color4 (Color4))
import My.Extra
import My.IO
import My.Prelude

white :: Color
white = Color4 1.0 1.0 1.0 1.0

main_character :: IO Texture
main_character = pictureFromFile $ $(makeRelativeToProject "assets/main_character.png" >>= strToExp)

floor_plate :: IO Texture
floor_plate = pictureFromFile $ $(makeRelativeToProject "assets/floor_plate.png" >>= strToExp)

vizualizeGame :: Texture -> Texture -> GameState -> Picture
vizualizeGame main_character' floor_plate' gameState =
  Pictures $
    [ TexturePlacements floor_plate' 1 1 $ (uncurry TexturePlacement . unCursorPos <$> (toList $ gsBoard gameState)),
      TexturePlacements main_character' 1 1 $ [uncurry TexturePlacement . unCursorPos $ gsMainCharacterPosition gameState],
      TexturePlacements main_character' 4 4 $ [TexturePlacement 0 0],
      TexturePlacements main_character' 4 4 $ [TexturePlacement 100 100]
    ]
