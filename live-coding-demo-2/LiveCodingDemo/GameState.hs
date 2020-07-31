module LiveCodingDemo.GameState where

import Data.Aeson (FromJSON, ToJSON)
import Data.List as L (zip)
import My.Prelude
import Playtime

data GameState = GameState
  { gsPlayer :: Pos,
    gsHearts :: [Pos],
    gsStars :: [(Double, Pos)],
    gsEnemies :: [Pos],
    gsSpeed :: Double
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

makeInitialGameState :: Dim -> Int -> GameState
makeInitialGameState dim (mkStdGen -> rng) =
  let numStars = 510
      (poss, rng') = randomPoss rng numStars dim
      (sizes, _) = randomsR rng' numStars (0, 4)
   in GameState
        { gsStars = sizes `zip` poss,
          gsEnemies = [],
          gsHearts = [],
          gsPlayer = (100, 100),
          gsSpeed = 1
        }

data TextureId = Heart | Plane | Enemy deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

textures :: TextureId -> (Scale, FilePath)
textures = \case
  Plane -> (1, "plane.png")
  Enemy -> (0.1, "enemy_red.png")
  Heart -> (0.025, "haskell_love_logo.png")

stepGameStatePure :: Int -> (TextureId -> Dim) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure (mkStdGen -> rng) area gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    let
     in -- (width,_) = esWindowDimensions
        --_randDoubles = int2Double . (`mod2`% width) <$> randInts
        gs
          { gsHearts = gsHearts <> ((repeat $ fst gsPlayer + 300) `zip` ((+ snd gsPlayer) <$>) [50, 100, 150, 200, 250, 300])
          }
  KeyEvent Key'P KeyState'Pressed ->
    let
     in -- (width,_) = esWindowDimensions
        --_randDoubles = int2Double . (`mod2`% width) <$> randInts
        gs
          { gsSpeed = gsSpeed + 1
          }
  KeyEvent Key'O KeyState'Pressed ->
    let
     in -- (width,_) = esWindowDimensions
        --_randDoubles = int2Double . (`mod2`% width) <$> randInts
        gs
          { gsSpeed = gsSpeed - 1
          }
  RenderEvent _ ->
    let speed = gsSpeed * esTimePassed
        velocity = 200
        step = esTimePassed * velocity
        (_, height) = esWindowDimensions
        moveX =
          if
              | Key'A `setMember` esKeysPressed -> subtract (step, 0)
              | Key'D `setMember` esKeysPressed -> (+ (step, 0))
              | True -> id
        moveY =
          if
              | Key'W `setMember` esKeysPressed -> subtract (0, step)
              | Key'S `setMember` esKeysPressed -> (+ (0, step))
              | True -> id
        moveStars (size, pos) = (size,) $ pos - (5 * speed * size, 0)
        maxEnemies = 10
        (bullets, killed) = unzip $ fmap (both (\(_, pos) -> pos)) $ filter (uncurry collidesWith) $ (,) <$> ((area Heart,) <$> gsHearts) <*> ((area Enemy,) <$> gsEnemies)
        newEnemies =
          take (maxEnemies - length gsEnemies) $
            repeat 1023 `zip` (fst $ randomsR rng maxEnemies (0, height))
        newEnemies' = filter (not . (`elem` killed)) $ gsEnemies <> newEnemies
     in gs
          { gsPlayer = moveX . moveY $ gsPlayer,
            gsEnemies =
              subtract (100 * speed, 0) . (`mod2` esWindowDimensions) <$> newEnemies',
            gsHearts =
              (+ (300 * speed, 0)) <$> filter ((< 1024) . fst) (gsHearts \\ bullets),
            gsStars =
              moveStars . (second (`mod2` esWindowDimensions)) <$> gsStars
          }
  _ -> gs
