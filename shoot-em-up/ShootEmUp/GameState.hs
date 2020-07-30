module ShootEmUp.GameState where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (zip)
import GHC.Float (double2Int)
import "GLFW-b" Graphics.UI.GLFW
import My.IO
import My.Prelude
import Playtime
import System.Random

data GameState = GameState
  { gsMainCharacter :: Pos,
    gsEnemies :: [Pos],
    gsStars :: [(Dim, Pos)],
    gsBullets :: [Pos],
    gsMaxStarSize :: Dim,
    gsDragAndDrop :: Maybe DragAndDrop
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

numEnemies :: Int
numEnemies = 10

data TextureId = Enemy | Heart | Plane
  deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

textures :: TextureId -> (Scale, FilePath)
textures = \case
  Plane -> (1, "plane.png")
  Enemy -> (0.1, "enemy_red.png")
  Heart -> (0.025, "haskell_love_logo.png")

makeInitialGameState :: Dim -> IO GameState
makeInitialGameState dim = do
  let maxStarSize = 4
  starX <- fmap (fmap fromIntegral) $ sequence $ replicate 510 $ randomRIO (0, maxStarSize + (double2Int $ unRelative $ fst dim))
  starY <- fmap (fmap fromIntegral) $ sequence $ replicate 510 $ randomRIO (0, maxStarSize + (double2Int $ unRelative $ snd dim))
  starSize <- fmap (fmap fromIntegral) $ sequence $ replicate 510 $ randomRIO (0, maxStarSize)
  pure
    GameState
      { gsMainCharacter = (10, 200),
        gsEnemies = mempty,
        gsStars = starSize `zip` (starX `zip` starY),
        gsBullets = mempty,
        gsMaxStarSize = fromIntegral maxStarSize,
        gsDragAndDrop = Nothing
      }

stepGameStatePure :: [Int] -> (TextureId -> Pos -> Area) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure pre area old_gs es event =
  foldl
    (&)
    old_gs
    [ \gs -> dragAndDrop es gs MouseButton'1 (getBulletAreas gs) setBullets (getDragAndDrop gs) setDragAndDrop event,
      \gs -> deleteOnClick es gs MouseButton'2 (getBulletAreas gs) setBullets event,
      \gs -> stepGameStatePure' pre area gs es event
    ]
  where
    setBullets bullets gs = gs {gsBullets = bullets}
    getBulletAreas gs = area Heart <$> gsBullets gs
    getDragAndDrop gs = gsDragAndDrop gs
    setDragAndDrop v gs = gs {gsDragAndDrop = v}

stepGameStatePure' :: [Int] -> (TextureId -> Pos -> Area) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure' randInts area gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    gs
      { gsBullets =
          gsBullets
            <> ((gsMainCharacter |+) <$> [(300, 100) :: Dim, (300, 145), (325, 200), (325, 290), (300, 340), (300, 390)])
      }
  RenderEvent _ ->
    let distancePerSec = 200
        (_, height) = esLogicalDimensions
        velocity :: Dim
        velocity =
          distancePerSec
            |*| ( if
                      | MovementAction Left' `setMember` esActions -> -1
                      | MovementAction Right' `setMember` esActions -> 1
                      | True -> 0 :: Factor X,
                  if
                      | MovementAction Up `setMember` esActions -> -1
                      | MovementAction Down `setMember` esActions -> 1
                      | True -> 0 :: Factor Y
                )
        bulletVelocity = (300, 0)
        bulletStep :: Dim
        bulletStep = esTimePassed *| bulletVelocity
        enemyDim = fst . area Enemy
        survivingEnemies =
          flip filter gsEnemies $ \enemyPos ->
            (fst enemyPos > - fst (originPos |- enemyDim enemyPos) &&)
              $ not
              $ any (area Enemy enemyPos `collidesWith`) (bulletTrajectory =<< gsBullets)
          where
            bulletTrajectory pos = area Heart <$> trajectoryPixels pos esTimePassed bulletVelocity
        newEnemies = survivingEnemies <> (modu . (1100,) . fromIntegral <$> take numAdded randInts)
          where
            numAdded = numEnemies - length survivingEnemies
            modu :: Pos -> Pos
            modu pos = pos |%| (height |- enemyDim pos)
        stepStar :: (Dim, Pos) -> (Dim, Pos)
        stepStar (size, pos) = (size,) $ modu $ move' pos
          where
            move' = (|- ((5, 0) :: Scale) |*| esTimePassed *| (size |+ (1 :: Dim)))
            modu = (|%%| (esLogicalDimensions |+ gsMaxStarSize))
     in gs
          { gsMainCharacter = gsMainCharacter |+ esTimePassed *| velocity,
            gsEnemies = (|- esTimePassed *| (100 :: Relative X)) <$> newEnemies,
            gsStars = stepStar <$> gsStars,
            gsBullets = filterX (< 1024) gsBullets <&> (|+ bulletStep)
          }
  _ -> gs
