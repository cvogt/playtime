module Playtime.Random where

import My.Prelude
import Playtime.Geometry
import System.Random

randomsR :: (Random a) => StdGen -> Int -> (a, a) -> ([a], StdGen)
randomsR = randomsR' . ([],)
  where
    randomsR' :: (Random a) => ([a], StdGen) -> Int -> (a, a) -> ([a], StdGen)
    randomsR' res n _ | n <= 0 = res
    randomsR' (acc, g') n r = randomsR' (first (: acc) $ randomR r g') (n -1) r

randomsNatDouble :: StdGen -> Int -> Double -> Double -> ([Double], StdGen)
randomsNatDouble g num minV maxV = randomsR g num (minV, maxV)

randomTuples :: StdGen -> Int -> Double -> Double -> Double -> Double -> ([(Double, Double)], StdGen)
randomTuples g n minX maxX minY maxY =
  let (xs, g') = randomsNatDouble g n minX maxX
      (ys, g'') = randomsNatDouble g' n minY maxY
   in (xs `zip` ys, g'')

randomPoss :: StdGen -> Int -> Dim -> ([Pos], StdGen)
randomPoss g n (maxX, maxY) = randomTuples g n 0 maxX 0 maxY

randomVelos :: StdGen -> Int -> Dim -> ([Velo], StdGen)
randomVelos g n (maxX, maxY) = randomTuples g n (- maxX) maxX (- maxY) maxY
