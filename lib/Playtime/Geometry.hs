module Playtime.Geometry where

import Data.List (zip)
import GHC.Float (int2Double)
import GHC.Real ((/), ceiling, floor)
import My.Prelude
import Playtime.Types

move :: Double -> Area -> Pos -> Double -> Double -> [Area] -> Pos
move timePassed (Area objectPos objectDim) previousPos velocityX velocityY obstacles =
  case lastMay unobstructed of
    Nothing -> objectPos
    Just pos ->
      let step (Pos cx'' cy'') (Pos cx' cy') =
            let goY = mfilter nonColliding $ Just $ Pos cx'' cy'
                goX = mfilter nonColliding $ Just $ Pos cx' cy''
                xOverY = x objectPos == x previousPos
             in if xOverY then goX <|> goY else goY <|> goX
       in case foldM step pos $ drop (length unobstructed) candidates of
            Nothing -> pos
            Just pos' -> pos'
  where
    nonColliding p = any (Area p objectDim `collidesWith`) obstacles
    candidates = trajectoryPixels objectPos timePassed velocityX velocityY
    unobstructed = takeWhile nonColliding candidates

-- given a position, a timedifference and x,y velocities - calculate relevant pixels along the trajector for checking collisions
trajectoryPixels :: Pos -> Double -> Double -> Double -> [Pos]
trajectoryPixels objectPos timePassed ((timePassed *) -> velocityX) ((timePassed *) -> velocityY) =
  -- FIXME: We should return a list of all the intersection points with pixel borders along the trajectory.
  --        What we currently do instead is wrong, but close enough for the moment.
  nub $ uncurry Pos <$> candidatesXY mx velocityX stepX `zip` candidatesXY my velocityY stepY
  where
    Pos mx my = objectPos
    steps :: Int
    steps = ceiling $ max (abs velocityX) (abs velocityY)
    stepX = velocityX / int2Double steps
    stepY = velocityY / int2Double steps
    candidatesXY :: Double -> Double -> Double -> [Double]
    candidatesXY base velocity step =
      (<> [base + velocity]) . toList $
        int2Double . (if step < 0 then floor else ceiling) <$> iterateN steps (+ step) base
