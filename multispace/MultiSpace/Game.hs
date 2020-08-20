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
  Player -> (3, "main_character.png")
  Enemy -> (0.1, "enemy_red.png")

data GameState = GameState
  { gsPlayer :: Pos,
    gsShip :: [Pos],
    gsPiloting :: Bool,
    gsEnemy :: [Pos]
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

makeInitialGameState :: Dim -> (TextureId -> Dim) -> Int -> GameState
makeInitialGameState dimensions tDim seed =
  let rng = mkStdGen seed
      (poss, _) = randomPoss rng 99 dimensions
   in GameState
        { gsPlayer = dimensions / 2,
          gsShip = [x * pixelsize + offset | x <- layout myFirstShipLayout],
          gsPiloting = False,
          gsEnemy = poss
        }

stepGameStatePure :: Int -> (TextureId -> Dim) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure seed tDim gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'E KeyState'Pressed -> gs {gsPiloting = not gsPiloting}
  RenderEvent _ ->
    let rng = mkStdGen seed
        speed = 200
        shipBounds = esDimensions - pixelsize
        newPoss = fmap (+ delta) gsShip
        shipCollide = any (\pos -> fst pos < 0 || snd pos < 0 || fst pos > fst shipBounds || snd pos > snd shipBounds) newPoss
        delta =
          (if Key'Up `setMember` esKeysPressed then (0, - speed * esTimePassed) else 0)
            + (if Key'Down `setMember` esKeysPressed then (0, speed * esTimePassed) else 0)
            + (if Key'Left `setMember` esKeysPressed then (- speed * esTimePassed, 0) else 0)
            + (if Key'Right `setMember` esKeysPressed then (speed * esTimePassed, 0) else 0)
        newPlayer =
          if (shipCollide && gsPiloting) || any (\x -> collidesWith (tDim Player, gsPlayer + delta) (pixelsize, x)) gsShip
            then gsPlayer
            else (gsPlayer + delta)
        newShipPos = if shipCollide then gsShip else newPoss
        remainingEnemies = filter (\e -> not $ collidesWith (tDim Player, gsPlayer) (tDim Enemy, e)) gsEnemy
     in gs
          { gsPlayer = newPlayer,
            gsShip = if gsPiloting then newShipPos else gsShip,
            gsEnemy = remainingEnemies
          }
  _ -> gs

visualize :: (TextureId -> Pos -> Sprite) -> EngineState -> GameState -> [Sprite]
visualize sprite EngineState {..} GameState {..} =
  let shipColor = if gsPiloting then (RGBA 0 255 0 255) else (RGBA 255 0 0 255)
   in [sprite Player gsPlayer]
        <> fmap (sprite Enemy) gsEnemy
        <> [rectangle Solid shipColor pixelsize x | x <- gsShip]

minPairWise :: (Double, Double) -> (Double, Double) -> (Double, Double)
minPairWise = pairWise min min

maxPairWise :: (Double, Double) -> (Double, Double) -> (Double, Double)
maxPairWise = pairWise max max

layout :: String -> [Pos]
layout s = layout' s 0 0

layout' :: String -> Double -> Double -> [Pos]
layout' [] _ _ = []
layout' ('#' : cs) x y = (x, y) : layout' cs (x + 1) y
layout' ('\n' : cs) x y = layout' cs 0 (y + 1)
layout' (_ : cs) x y = layout' cs (x + 1) y

offset :: Pos
offset = 150

pixelsize :: Dim
pixelsize = 36

myFirstShipLayout :: String
myFirstShipLayout =
  "\
  \#################\n\
  \#       #       #\n\
  \#       #       #\n\
  \#       #       #\n\
  \#       #       #\n\
  \#       #       #\n\
  \#               #\n\
  \#               #\n\
  \#               #\n\
  \#               #\n\
  \#               #\n\
  \#               #\n\
  \#################\n\
  \"
