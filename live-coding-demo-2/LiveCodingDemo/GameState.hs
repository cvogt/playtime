module LiveCodingDemo.GameState where

import Data.Aeson (FromJSON, ToJSON)
import Data.List as L (zip)
import My.Prelude
import Playtime

data GameState = GameState
  { gsPlayer :: Pos,
    gsBullets :: [Pos],
    gsStars :: [(Dim, Pos)],
    gsEnemies :: [Pos],
    gsSpeed :: Scale
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

makeInitialGameState :: Dim -> Int -> GameState
makeInitialGameState dim (mkStdGen -> rng) =
  let numStars = 510
      (poss, rng') = randomPoss rng numStars dim
      (sizes, _) = randomRects rng' numStars 4
   in GameState
        { gsStars = sizes `zip` poss,
          gsEnemies = [],
          gsBullets = [],
          gsPlayer = (100, 100),
          gsSpeed = 1
        }

data TextureId = Heart | Plane | Enemy deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

textures :: TextureId -> (Scale, FilePath)
textures = \case
  Plane -> (1, "plane.png")
  Enemy -> (0.1, "enemy_red.png")
  Heart -> (0.025, "haskell_love_logo.png")

stepGameStatePure :: Int -> (TextureId -> Pos -> Area) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure (mkStdGen -> rng) area gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    let
     in -- (width,_) = esLogicalDimensions
        --_randDoubles = int2Double . (|%| width) <$> randInts
        gs
          { gsBullets = gsBullets <> ((repeat $ fst gsPlayer + 300) `zip` ((+ snd gsPlayer) <$>) [50, 100, 150, 200, 250, 300])
          }
  KeyEvent Key'P KeyState'Pressed ->
    let
     in -- (width,_) = esLogicalDimensions
        --_randDoubles = int2Double . (|%| width) <$> randInts
        gs
          { gsSpeed = gsSpeed + 1
          }
  KeyEvent Key'O KeyState'Pressed ->
    let
     in -- (width,_) = esLogicalDimensions
        --_randDoubles = int2Double . (|%| width) <$> randInts
        gs
          { gsSpeed = gsSpeed - 1
          }
  RenderEvent _ ->
    let speed = gsSpeed |*| esTimePassed
        velocity = 200 :: Dim
        step = esTimePassed *| velocity
        (width, height) = esLogicalDimensions
        moveX =
          if
              | Key'A `setMember` esKeysPressed -> (|- fst step)
              | Key'D `setMember` esKeysPressed -> (|+ fst step)
              | True -> id
        moveY =
          if
              | Key'W `setMember` esKeysPressed -> (|- snd step)
              | Key'S `setMember` esKeysPressed -> (|+ snd step)
              | True -> id
        moveStars (size, pos) = (size,) $ pos |- (5 :: Relative X) |*| speed |*| (fst size & \(Relative v) -> xFactor v)
        maxEnemies = 10
        (bullets, killed) = unzip $ fmap (both (\(_, pos) -> pos)) $ filter (uncurry collidesWith) $ (,) <$> (area Heart <$> gsBullets) <*> (area Enemy <$> gsEnemies)
        newEnemies =
          take (maxEnemies - length gsEnemies) $
            (repeat 1023)
              `zip` (fst $ randomsAbsoluteY rng maxEnemies height)
        newEnemies' = filter (not . (`elem` killed)) $ gsEnemies <> newEnemies
     in gs
          { gsPlayer = moveX . moveY $ gsPlayer,
            gsEnemies =
              (|- (100 :: Relative X) |*| speed) . (|%%| width) <$> newEnemies',
            gsBullets =
              (|+ (300 :: Relative X) |*| speed) <$> filter ((< 1024) . fst) (gsBullets \\ bullets),
            gsStars =
              moveStars . (second (|%%| width)) <$> gsStars
          }
  _ -> gs
