module Playtime.Util where

import Data.List (zip)
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Clock.TAI
import GHC.Float (double2Int, int2Double)
import GHC.Real ((/))
import My.Prelude
import Playtime.Types
import System.Random

timeDiffPico :: SystemTime -> SystemTime -> Integer
timeDiffPico before after = diffTimeToPicoseconds $ diffAbsoluteTime (systemToTAITime after) (systemToTAITime before)

pico2second :: Double -> Double
pico2second picosecs = picosecs / 1000 / 1000 / 1000 / 1000

pico2Double :: Integral i => i -> Double
pico2Double pico = int2Double (fromIntegral pico) / 1000 / 1000 / 1000 / 1000

avg :: Foldable t => t Integer -> Double
avg xs = (fromInteger @Double $ sum xs) / (int2Double $ length xs)

translate :: Dim -> Sprite -> Sprite
translate offset (Rectangle (dim, pos) v) = Rectangle (dim, pos |+ offset) v

allEnumValues :: forall a. (Enum a, Bounded a) => [a]
allEnumValues = enumFrom (minBound :: a)

randomsR :: (Random a) => StdGen -> Int -> (a, a) -> ([a], StdGen)
randomsR = randomsR' . ([],)
  where
    randomsR' :: (Random a) => ([a], StdGen) -> Int -> (a, a) -> ([a], StdGen)
    randomsR' res n _ | n <= 0 = res
    randomsR' (acc, g') n r = randomsR' (first (: acc) $ randomR r g') (n -1) r

randomPoss :: StdGen -> Int -> Dim -> ([Pos], StdGen)
randomPoss g n (maxX, maxY) =
  let (xs, g') = randomsAbsoluteX g n maxX
      (ys, g'') = randomsAbsoluteY g' n maxY
   in (xs `zip` ys, g'')

randomDims :: StdGen -> Int -> Dim -> ([Dim], StdGen)
randomDims g n (maxX, maxY) =
  let (xs, g') = randomsRelativeX g n maxX
      (ys, g'') = randomsRelativeY g' n maxY
   in (xs `zip` ys, g'')

randomRects :: StdGen -> Int -> Int -> ([Dim], StdGen)
randomRects g n maxXY =
  let (xs, g') = randomsR g n (0, maxXY)
   in ((xRelative . int2Double <$> xs) `zip` (yRelative . int2Double <$> xs), g')

randomScales :: StdGen -> Int -> Scale -> ([Scale], StdGen)
randomScales g n (maxX, maxY) =
  let (xs, g') = randomsFactorX g n maxX
      (ys, g'') = randomsFactorY g' n maxY
   in (xs `zip` ys, g'')

randomsAbsoluteX :: StdGen -> Int -> Relative X -> ([Absolute X], StdGen)
randomsAbsoluteX g n (Relative maxX) =
  let (xs, g') = randomsR g n (0, double2Int maxX)
   in (xAbsolute . int2Double <$> xs, g')

randomsAbsoluteY :: StdGen -> Int -> Relative Y -> ([Absolute Y], StdGen)
randomsAbsoluteY g n (Relative maxY) =
  let (ys, g') = randomsR g n (0, double2Int maxY)
   in (yAbsolute . int2Double <$> ys, g')

randomsRelativeX :: StdGen -> Int -> Relative X -> ([Relative X], StdGen)
randomsRelativeX g n (Relative maxX) =
  let (xs, g') = randomsR g n (0, double2Int maxX)
   in (xRelative . int2Double <$> xs, g')

randomsRelativeY :: StdGen -> Int -> Relative Y -> ([Relative Y], StdGen)
randomsRelativeY g n (Relative maxY) =
  let (ys, g') = randomsR g n (0, double2Int maxY)
   in (yRelative . int2Double <$> ys, g')

randomsFactorX :: StdGen -> Int -> Factor X -> ([Factor X], StdGen)
randomsFactorX g n (Factor maxX) =
  let (xs, g') = randomsR g n (0, double2Int maxX)
   in (xFactor . int2Double <$> xs, g')

randomsFactorY :: StdGen -> Int -> Factor Y -> ([Factor Y], StdGen)
randomsFactorY g n (Factor maxY) =
  let (ys, g') = randomsR g n (0, double2Int maxY)
   in (yFactor . int2Double <$> ys, g')
