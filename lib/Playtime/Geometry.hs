module Playtime.Geometry where

import Data.List (zip)
import GHC.Float (int2Double)
import GHC.Real (ceiling, floor)
import My.Prelude
import Playtime.Types

textureDim :: (a -> (Scale, b)) -> (b -> Texture) -> a -> Dim
textureDim textureScale textures i =
  let (scale, b) = textureScale i
      Texture dim _ _ = textures b
   in scale * dim

move :: Double -> Area -> Pos -> Double -> Double -> [Area] -> Pos
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
trajectoryPixels :: Pos -> Double -> Dim -> [Pos]
trajectoryPixels objectPos timePassed ((dupe timePassed *) -> (velocityX, velocityY)) =
  -- FIXME: We should return a list of all the intersection points with pixel borders along the trajectory.
  --        What we currently do instead is wrong, but close enough for the moment.
  nub $ candidatesXY mx velocityX stepX `zip` candidatesXY my velocityY stepY
  where
    (mx, my) = objectPos
    steps :: Int
    steps = ceiling $ max (abs velocityX) (abs velocityY)
    stepX = velocityX / int2Double steps
    stepY = velocityY / int2Double steps
    candidatesXY :: Double -> Double -> Double -> [Double]
    candidatesXY base velocity step =
      (<> [base + velocity]) . toList $
        int2Double
          . (if step < 0 then floor else ceiling)
          <$> iterateN steps (+ step) base
