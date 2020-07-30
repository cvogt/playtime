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
randomPoss g n (width, height) =
  let (xs, g') = randomsAbsoluteX g n width
      (ys, g'') = randomsAbsoluteY g' n height
   in (xs `zip` ys, g'')

randomsAbsoluteX :: StdGen -> Int -> Relative X -> ([Absolute X], StdGen)
randomsAbsoluteX g n (Relative width) =
  let (xs, g') = randomsR g n (0, double2Int width)
   in (xAbsolute . int2Double <$> xs, g')

randomsAbsoluteY :: StdGen -> Int -> Relative Y -> ([Absolute Y], StdGen)
randomsAbsoluteY g n (Relative height) =
  let (ys, g') = randomsR g n (0, double2Int height)
   in (yAbsolute . int2Double <$> ys, g')
