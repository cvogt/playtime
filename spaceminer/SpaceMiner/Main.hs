module Main where

import qualified Data.Map as Map
import My.IO
import My.Prelude
import Playtime
import Playtime.Types
import SpaceMiner.GameState
import SpaceMiner.Graphics

--   foldl (.) id (flap [applyToEngineState, applyToGameState] event')
main :: IO ()
main =
  let igs = makeInitialGameState dim
      dim = Dimensions {width = 320, height = 240} -- logical pixel resolution
   in playtime $ EngineConfig igs dim stepGameState' computeSpritePlacements $ \gs ->
        let GameState {..} = gs
            Pos x' y' = gsMainCharacterPosition
         in [ "collisions: " <> show gsCollisions,
              "main char: " <> show (x', y'),
              "last places sprite location: " <> show gsLastPlacement,
              "sprite count floor: " <> show (Map.size $ unBoard gsFloor),
              "sprite count room: " <> show (Map.size $ unBoard gsRoom)
            ]
