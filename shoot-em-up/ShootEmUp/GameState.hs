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
    gsHearts :: [Pos],
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
        gsHearts = mempty,
        gsMaxStarSize = fromIntegral maxStarSize,
        gsDragAndDrop = Nothing
      }

stepGameStatePure :: [Int] -> (TextureId -> Dim) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure pre tdim old_gs es event =
  foldl
    (&)
    old_gs
    [ \gs -> dragAndDrop es gs MouseButton'1 (getBulletAreas gs) setBullets (getDragAndDrop gs) setDragAndDrop event,
      \gs -> deleteOnClick es gs MouseButton'2 (getBulletAreas gs) setBullets event,
      \gs -> stepGameStatePure' pre tdim gs es event
    ]
  where
    setBullets bullets gs = gs {gsHearts = bullets}
    getBulletAreas gs = (tdim Heart,) <$> gsHearts gs
    getDragAndDrop gs = gsDragAndDrop gs
    setDragAndDrop v gs = gs {gsDragAndDrop = v}

stepGameStatePure' :: [Int] -> (TextureId -> Dim) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure' randInts tdim gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    gs
      { gsHearts =
          gsHearts
            <> ((gsMainCharacter |+) <$> [(300, 100) :: Dim, (300, 145), (325, 200), (325, 290), (300, 340), (300, 390)])
      }
  RenderEvent _ ->
    let distancePerSec = 200
        (_, height) = esWindowDimensions
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
        survivingEnemies =
          flip filter gsEnemies $ \enemyPos ->
            (fst enemyPos > - fst (originPos |- tdim Enemy) &&)
              $ not
              $ any ((tdim Enemy, enemyPos) `collidesWith`) (bulletTrajectory =<< gsHearts)
          where
            bulletTrajectory pos = (tdim Heart,) <$> trajectoryPixels pos esTimePassed bulletVelocity
        newEnemies = survivingEnemies <> (modu . (1100,) . fromIntegral <$> take numAdded randInts)
          where
            numAdded = numEnemies - length survivingEnemies
            modu :: Pos -> Pos
            modu = (|%% (height |- tdim Enemy))
        stepStar :: (Dim, Pos) -> (Dim, Pos)
        stepStar (size, pos) = (size,) $ modu $ move' pos
          where
            move' = (|- ((5, 0) :: Scale) |*| esTimePassed *| (size |+ (1 :: Dim)))
            modu = (|% (esWindowDimensions |+ gsMaxStarSize))
     in gs
          { gsMainCharacter = gsMainCharacter |+ esTimePassed *| velocity,
            gsEnemies = (|- esTimePassed *| (100 :: Relative X)) <$> newEnemies,
            gsStars = stepStar <$> gsStars,
            gsHearts = filterX (< 1024) gsHearts <&> (|+ bulletStep)
          }
  _ -> gs
