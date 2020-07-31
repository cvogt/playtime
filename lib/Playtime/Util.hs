module Playtime.Util where

import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Clock.TAI
import GHC.Float (int2Double)
import GHC.Real ((/))
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

translate :: Dim -> Sprite -> Sprite
translate offset (Rectangle (dim, pos) v) = Rectangle (dim, pos |+ offset) v

allEnumValues :: forall a. (Enum a, Bounded a) => [a]
allEnumValues = enumFrom (minBound :: a)

---

groupKeyBindings :: [([Key], Action)] -> Map Key [(Set Key, Action)]
groupKeyBindings keyBindingsRaw = mapFromList $ groups <&> \l@(h :| _) -> (fst h, first setFromList <$> (join . toList $ snd <$> l))
  where
    groups :: [NonEmpty (Key, [([Key], Action)])]
    groups = groupAllWith fst $ join $ keyBindingsRaw <&> (\b@(keys', _) -> (,[b]) <$> keys')

keyBindings :: [([Key], Action)]
keyBindings =
  [ ([Key'LeftSuper, Key'Q], Exit),
    ([Key'Escape], Exit),
    ([Key'LeftSuper, Key'L], OneTimeEffect Load),
    ([Key'LeftSuper, Key'S], OneTimeEffect Save),
    ([Key'LeftSuper, Key'R], OneTimeEffect Reset),
    ([Key'W], MovementAction Up),
    ([Key'S], MovementAction Down),
    ([Key'A], MovementAction Left'),
    ([Key'D], MovementAction Right')
  ]
