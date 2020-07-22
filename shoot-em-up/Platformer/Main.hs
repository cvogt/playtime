module Platformer.Main where

import My.IO
import My.Prelude
import Platformer.GameState
import Platformer.Graphics
import Playtime
import Playtime.Types

--   foldl (.) id (flap [applyToEngineState, applyToGameState] event')
main :: IO ()
main =
  let igs = makeInitialGameState dim
      dim = Dimensions {width = 1024, height = 768} -- logical pixel resolution
   in playtime $ EngineConfig igs dim 1 stepGameState' computeSpritePlacements $ \EngineState {..} GameState {..} ->
        let Pos x' y' = gsMainCharacterPosition
         in [ "main char: " <> show (x', y'),
              "bullets: " <> show gsBullets,
              "enemies: " <> show gsEnemies
            ]

tests :: IO ()
tests = pure ()
