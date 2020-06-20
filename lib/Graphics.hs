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

loadPic :: IO Picture
loadPic = pictureFromFile $ $(makeRelativeToProject "assets/main_character.png" >>= strToExp)

vizualizeGame :: Picture -> GameState -> IO Picture
vizualizeGame pic gameState = do
  let g2gp = \(CursorPos (x, y)) -> (int2Float $ xg2g x, int2Float $ yg2g y)
  let board = fmap g2gp . toList $ gsBoard gameState
  -- let blueSquare = Color blue $ Polygon [(0, 0), (0, 50), (50, 50), (50, 0)]
  let CursorPos (x, y) = gsCursorPos gameState
  let CursorPos (x', y') = gsMainCharacterPosition gameState
  pure $ Pictures $
    [ (uncurry Translate (g2gp $ gsMainCharacterPosition gameState) pic),
      Translate 0 100 $ Scale 0.2 0.2 $ Text white $ show (xg2g x, yg2g y),
      --, Color blue $ Polygon [(0,0),(0,50),(50,50),(50,0)]
      Translate 0 25 $ Scale 0.2 0.2 $ Text white $ "keys: " <> (show $ gsKeysPressed gameState),
      Translate 0 50 $ Scale 0.2 0.2 $ Text white $ "char: " <> show (x', y'),
      Translate 0 75 $ Scale 0.2 0.2 $ Text white $ "char g2g: " <> show (g2gp $ gsMainCharacterPosition gameState),
      Translate 0 125 $ Scale 0.2 0.2 $ Text white $ "poss: " <> show (Set.size $ gsBoard gameState),
      Translate 0 150 $ Scale 0.2 0.2 $ Text white $ "fps: " <> show (gsFps gameState),
      Scale 0.2 0.2 $ Text white $ "openGL: " <> show (x, y)
    ]
      <> ((uncurry Translate <$> board) <&> ($ pic))

xg2g :: Int -> Int
xg2g x = (x - 320)

yg2g :: Int -> Int
yg2g y = (y - 240) * (-1)

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
