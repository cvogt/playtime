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

makeInitialGameState :: Dim -> IO GameState
makeInitialGameState dim = do
  let maxStarSize = 4
  starX <- fmap (fmap fromIntegral) $ sequence $ replicate 510 $ randomRIO (0, maxStarSize + (double2Int $ unRelative $ fst dim))
  starY <- fmap (fmap fromIntegral) $ sequence $ replicate 510 $ randomRIO (0, maxStarSize + (double2Int $ unRelative $ snd dim))
  pure
    GameState
      { gsStars = starX `zip` starY,
        gsEnemies = [],
        gsBullets = [],
        gsPlayer = (100, 100)
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
    let
     in -- (width,_) = esLogicalDimensions
        --_randDoubles = int2Double . (|%| width) <$> randInts
        gs
          { gsBullets = gsBullets <> ((repeat $ fst gsPlayer + 300) `zip` ((+ snd gsPlayer) <$>) [50, 100, 150, 200, 250, 300])
          }
  RenderEvent _ ->
    let velocity = (200, 200) :: Dim
        step = esTimePassed *| velocity
        (width, height) = esLogicalDimensions
        moveX = if Key'A `setMember` esKeysPressed then (|- step) else if Key'D `setMember` esKeysPressed then (|+ step) else id
        moveY = if Key'W `setMember` esKeysPressed then (|- step) else if Key'S `setMember` esKeysPressed then (|+ step) else id
        randDoubles = Absolute . int2Double . (flip mod $ double2Int $ unRelative height) <$> randInts
        bulletVelocityX :: Relative X
        bulletVelocityX = 300
        (bullets, killed) = unzip $ fmap (both (\(_, pos) -> pos)) $ filter (uncurry collidesWith) $ (,) <$> (area Heart <$> gsBullets) <*> (area Enemy <$> gsEnemies)
        newEnemies = take (10 - length gsEnemies) $ (repeat 1023) `zip` drop 10 randDoubles
        newEnemies' = filter (not . (`elem` killed)) $ gsEnemies <> newEnemies
     in gs
          { gsPlayer = updateY moveY $ updateX moveX gsPlayer,
            gsEnemies =
              (|- (50 :: Relative X) |*| esTimePassed) . (|%%| width) <$> newEnemies',
            gsBullets =
              (|+ bulletVelocityX |*| esTimePassed) <$> filter ((< 1024) . fst) (gsBullets \\ bullets),
            gsStars =
              (|- (20 :: Relative X) |*| esTimePassed) . (|%%| width) <$> gsStars
          }
  _ -> gs
