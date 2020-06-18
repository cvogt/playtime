module Music where

import Data.FileEmbed
import Euterpea hiding (Text, forever)
import My.IO
import My.Prelude
import System.Posix.Process (executeFile)

spawnSynth :: IO ()
spawnSynth =
  executeFile "/usr/local/bin/fluidsynth" False [$(makeRelativeToProject "Steinway+Grand+Piano+ER3A.sf2" >>= strToExp)] Nothing

playMusic :: IO ()
playMusic = do
  let p1 = [c 5 qn, e 5 qn, d 5 en, c 5 en, e 5 qn, c 5 qn, b 4 en, e 5 en, a 4 hn]
  let p2 = [e 5 qn, e 5 qn, g 5 qn, g 5 en, a 5 en, f 5 qn, a 5 qn, e 5 hn]
  let p3 = [c 6 qn, b 5 en, a 5 en, b 5 qn, e 5 qn, a 5 qn, e 5 en, d 5 en, e 5 qn, b 4 qn]
  let p = Euterpea.play . Euterpea.line
  p $ p1 <> p2 <> p3
  p $ [chord [c 4 en, e 4 en, g 4 en], chord [c 4 en, e 4 en, g 4 en], chord [c 4 en, e 4 en, g 4 en], chord [a 3 hn, c 4 hn, e 4 hn]] -- p1 <> p2 <> p3
