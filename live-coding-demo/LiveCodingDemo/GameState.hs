module LiveCodingDemo.GameState where

import Data.Aeson (FromJSON, ToJSON)
import Data.List as L (zip)
import GHC.Float (double2Int, int2Double)
import GHC.Real (mod)
import My.IO
import My.Prelude
--import Playtime.Geometry
import Playtime
import System.Random

data GameState = GameState
  { gsPlayer :: Pos,
    gsBullets :: [Pos],
    gsStars :: [Pos],
    gsEnemies :: [Pos]
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

makeInitialGameState :: Dimensions -> IO GameState
makeInitialGameState Dimensions {width} = do
  randInts :: [Int] <- sequence $ replicate 1000 randomIO
  let randDoubles = int2Double . (`mod` double2Int width) <$> randInts
  pure
    GameState
      { gsStars = uncurry Pos <$> (take 500 randDoubles `zip` drop 500 randDoubles),
        gsEnemies = [],
        gsBullets = [],
        gsPlayer = Pos 100 100
      }

data TextureId = Heart | Plane | Enemy deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

textures :: TextureId -> (Scale, FilePath)
textures = \case
  Plane -> (1, "plane.png")
  Enemy -> (0.1, "enemy_red.png")
  Heart -> (0.025, "haskell_love_logo.png")

stepGameStatePure :: [Int] -> (TextureId -> Pos -> Area) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure randInts area gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    let Dimensions {width} = esLogicalDimensions
        _randDoubles = int2Double . (`mod` double2Int width) <$> randInts
     in gs
          { gsBullets = gsBullets <> (uncurry Pos <$> ((toList $ repeat $ x gsPlayer + 300) `zip` ((+ y gsPlayer) <$>) [50, 100, 150, 200, 250, 300]))
          }
  RenderEvent _ ->
    let velocity = 200
        step = esTimePassed * velocity
        Dimensions {height, width} = esLogicalDimensions
        moveX = if Key'A `setMember` esKeysPressed then subtract step else if Key'D `setMember` esKeysPressed then (+ step) else id
        moveY = if Key'W `setMember` esKeysPressed then subtract step else if Key'S `setMember` esKeysPressed then (+ step) else id
        randDoubles = int2Double . (`mod` double2Int height) <$> randInts
        bulletVelocityX = 300
        (bullets, killed) = unzip $ fmap (both (\(pos, _) -> pos)) $ filter (uncurry collidesWith) $ (,) <$> (area Heart <$> gsBullets) <*> (area Enemy <$> gsEnemies)
        newEnemies = take (10 - length gsEnemies) $ uncurry Pos <$> ((toList $ repeat 1023) `zip` drop 10 randDoubles)
        newEnemies' = filter (not . (`elem` killed)) $ gsEnemies <> newEnemies
     in gs
          { gsPlayer = updateY moveY $ updateX moveX gsPlayer,
            gsEnemies =
              updateX (subtract $ 50 * esTimePassed) . updateX (`mod'` width) <$> newEnemies',
            gsBullets =
              updateX (+ bulletVelocityX * esTimePassed) <$> filter ((< 1024) . x) (gsBullets \\ bullets),
            gsStars =
              updateX (subtract $ 20 * esTimePassed) . updateX (`mod'` width) <$> gsStars
          }
  _ -> gs
