module Playtime.Util where

import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Clock.TAI
import GHC.Float (int2Double)
import GHC.Num (fromInteger)
import GHC.Real ((/), fromIntegral)
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

translate :: Pos -> Sprite -> Sprite
translate (Pos xd yd) (Rectangle (Pos x y, dim) v) = Rectangle (Pos (x + xd) (y + yd), dim) v

allEnumValues :: forall a. (Enum a, Bounded a) => [a]
allEnumValues = enumFrom (minBound :: a)
