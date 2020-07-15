module Main where

import qualified Data.Map as Map
import My.IO
import My.Prelude
import Platformer.GameState
import Platformer.Graphics
import Playtime
import Playtime.Types
import Playtime.Util

--   foldl (.) id (flap [applyToEngineState, applyToGameState] event')
main :: IO ()
main =
  let igs = makeInitialGameState dim
      dim = Dimensions {width = 320, height = 240} -- logical pixel resolution
   in playtime $ EngineConfig igs dim stepGameState' computeSpritePlacements $ \EngineState {..} GameState {..} ->
        let Pos x' y' = gsMainCharacterPosition
         in [ "gsJumped: " <> show (gsJumped),
              "gsJumped: " <> show (pico2Double . timeDiffPico gsLastLoopTime <$> gsJumped),
              "collisions: " <> show gsCollisions,
              "main char: " <> show (x', y'),
              "sprite count floor: " <> show (Map.size $ unBoard gsFloor),
              "sprite count room: " <> show (Map.size $ unBoard gsRoom)
            ]
