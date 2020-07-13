module Main where

import My.IO
import qualified SpaceMiner

main :: IO ()
main = SpaceMiner.main

--   foldl (.) id (flap [applyToEngineState, applyToGameState] event')
