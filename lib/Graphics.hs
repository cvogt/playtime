{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}

module Graphics where

import Bitmap
import Data.FileEmbed
import qualified Data.Set as Set
import GHC.Float (int2Float)
import GLFWHelpers
import Game
import Graphics.Rendering.OpenGL.GL (Color4 (Color4))
import My.IO
import My.Prelude

white :: Color
white = Color4 1.0 1.0 1.0 1.0

loadPic :: IO Texture
loadPic = pictureFromFile $ $(makeRelativeToProject "assets/main_character.png" >>= strToExp)

vizualizeGame :: Texture -> GameState -> Picture
vizualizeGame pic gameState =
  Pictures $
    [ TexturePlacements pic 4 4 $ [cursor2texture $ gsMainCharacterPosition gameState],
      TexturePlacements pic 4 4 $ [TexturePlacement 0 0],
      TexturePlacements pic 4 4 $ [TexturePlacement 100 100],
      TexturePlacements pic 1 1 $ (fmap cursor2texture . toList $ gsBoard gameState)
    ]

cursor2texture :: CursorPos -> TexturePlacement
cursor2texture (CursorPos (int2Float -> x, int2Float -> y)) = TexturePlacement x y

-- move with cursor -- Translate (int2Float $ gridify x') (int2Float $ gridify y')

-- grid =
--   fixPolyPos $ Color white $ Pictures $
--     (([1 .. (windowWidth / gridsize)]) <&> \((* gridsize) -> x) -> Line [(x, 0), (x, windowHeight)])
--       <> (([1 .. (windowHeight / gridsize)]) <&> \((* gridsize) -> y) -> Line [(0, y), (windowWidth, y)])
-- fixPolyPos = Translate (- (windowWidth / 2)) (- (windowHeight / 2))
-- gridsize :: Float
-- gridsize = 20
-- gridify :: Int -> Int
-- gridify = (* (float2Int gridsize)) . round . (/ gridsize) . int2Float
