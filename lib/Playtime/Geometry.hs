module Playtime.Geometry where

import Data.List (zip)
import GHC.Float (int2Double)
import GHC.Real (ceiling, floor)
import My.Prelude
import Playtime.Types

textureArea :: (a -> (Scale, b)) -> (b -> Texture) -> a -> Pos -> Area
textureArea textureScale textures i pos =
  let (scale, b) = textureScale i
      Texture dim _ _ = textures b
   in (,pos) $ scale *| dim

updateX :: (Absolute X -> Absolute X) -> Pos -> Pos
updateX f (x, y) = (f x, y)

updateY :: (Absolute Y -> Absolute Y) -> Pos -> Pos
updateY f (x, y) = (x, f y)

updateXIf :: (Absolute X -> Bool) -> (Absolute X -> Absolute X) -> Pos -> Pos
updateXIf c f (x, y) = if c x then (f x, y) else (x, y)

filterX :: (Absolute X -> Bool) -> [Pos] -> [Pos]
filterX f = filter (f . fst)

move :: Scale -> Area -> Pos -> Relative X -> Relative Y -> [Area] -> Pos
move timePassed (objectDim, objectPos) previousPos velocityX velocityY obstacles =
  case lastMay unobstructed of
    Nothing -> objectPos
    Just pos ->
      let step (cx'', cy'') (cx', cy') =
            let goY = mfilter nonColliding $ Just $ (cx'', cy')
                goX = mfilter nonColliding $ Just $ (cx', cy'')
                xOverY = fst objectPos == fst previousPos
             in if xOverY then goX <|> goY else goY <|> goX
       in case foldM step pos $ drop (length unobstructed) candidates of
            Nothing -> pos
            Just pos' -> pos'
  where
    nonColliding p = not $ any ((objectDim, p) `collidesWith`) obstacles
    candidates = trajectoryPixels objectPos timePassed (velocityX, velocityY)
    unobstructed = takeWhile nonColliding candidates

-- given a position, a timedifference and x,y velocities - calculate relevant pixels along the trajector for checking collisions
trajectoryPixels :: Pos -> Scale -> Dim -> [Pos]
trajectoryPixels objectPos timePassed ((timePassed *|) -> (velocityX, velocityY)) =
  -- FIXME: We should return a list of all the intersection points with pixel borders along the trajectory.
  --        What we currently do instead is wrong, but close enough for the moment.
  nub $ candidatesXY mx velocityX stepX `zip` candidatesXY my velocityY stepY
  where
    (mx, my) = objectPos
    steps :: Int
    steps = ceiling $ max (abs $ unRelative velocityX) (abs $ unRelative velocityY)
    stepX = velocityX |/ (Factor @X $ int2Double steps)
    stepY = velocityY |/ (Factor @Y $ int2Double steps)
    candidatesXY :: Absolute a -> Relative a -> Relative a -> [Absolute a]
    candidatesXY base velocity step =
      (<> [base |+| velocity]) . toList $
        Absolute . int2Double
          . (if step < 0 then floor else ceiling)
          . unAbsolute
          <$> iterateN steps (|+| step) base
