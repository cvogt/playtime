module Playtime.Util where

import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Clock.TAI
import GHC.Float (int2Double)
import GHC.Num (fromInteger)
import GHC.Real ((/), fromIntegral)
import My.IO
import My.Prelude
import Playtime.Types

timeDiffPico :: SystemTime -> SystemTime -> Integer
timeDiffPico before after = diffTimeToPicoseconds $ diffAbsoluteTime (systemToTAITime after) (systemToTAITime before)

pico2second :: Double -> Double
pico2second picosecs = picosecs / 1000 / 1000 / 1000 / 1000

pico2Double :: Integral i => i -> Double
pico2Double pico = int2Double (fromIntegral pico) / 1000 / 1000 / 1000 / 1000

avg :: Foldable t => t Integer -> Double
avg xs = (fromInteger @Double $ sum xs) / (int2Double $ length xs)

translate :: Pos -> TexturePlacements -> TexturePlacements
translate (Pos xd yd) (TexturePlacements t (Area (Pos x y) dim)) = TexturePlacements t $ Area (Pos (x + xd) (y + yd)) dim
translate (Pos xd yd) (Rectangle mode (Area (Pos x y) dim) c) = Rectangle mode (Area (Pos (x + xd) (y + yd)) dim) c

noPostStepIO :: EngineState -> gameState -> IO gameState
noPostStepIO _ gs = pure gs

noPreStepIO :: EngineState -> IO ()
noPreStepIO _ = pure ()
