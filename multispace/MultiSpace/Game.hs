{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module MultiSpace.Game where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Err (error)
import My.Prelude
import Playtime

data TextureId = Enemy | Player
  deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

textures :: TextureId -> (Scale, FilePath)
textures = \case
  Player -> (0.3, "enemy_red.png")
  Enemy -> (0.1, "enemy_red.png")

data GameState = GameState
  { gsPlayer :: Pos,
    gsEnemy :: Pos
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

makeInitialGameState :: Dim -> Int -> GameState
makeInitialGameState dimensions seed =
  let rng = mkStdGen seed
      (poss, _) = randomPoss rng 99 dimensions
   in GameState
        { gsPlayer = dimensions / 2,
          gsEnemy = fromMaybe (error "no enemy position") $ headMay poss
        }

stepGameStatePure :: Int -> (TextureId -> Dim) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure seed tDim gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    gs
  RenderEvent _ ->
    let rng = mkStdGen seed
        speed = 200
        bounds = esDimensions - tDim Player
        delta =
          (if Key'Up `setMember` esKeysPressed then (0, - speed * esTimePassed) else 0)
            + (if Key'Down `setMember` esKeysPressed then (0, speed * esTimePassed) else 0)
            + (if Key'Left `setMember` esKeysPressed then (- speed * esTimePassed, 0) else 0)
            + (if Key'Right `setMember` esKeysPressed then (speed * esTimePassed, 0) else 0)
     in gs {gsPlayer = maxPairWise 0 $ minPairWise bounds $ gsPlayer + delta}
  _ -> gs

visualize :: (TextureId -> Pos -> Sprite) -> EngineState -> GameState -> [Sprite]
visualize sprite EngineState {..} GameState {..} =
  let
   in [sprite Player gsPlayer, sprite Enemy gsEnemy]

minPairWise :: (Double, Double) -> (Double, Double) -> (Double, Double)
minPairWise = pairWise min min

maxPairWise :: (Double, Double) -> (Double, Double) -> (Double, Double)
maxPairWise = pairWise max max
