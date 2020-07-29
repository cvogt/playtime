module ShootEmUp.GameState where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (zip)
import GHC.Float (double2Int, int2Double)
import GHC.Real (mod)
import "GLFW-b" Graphics.UI.GLFW
import My.IO
import My.Prelude
import Playtime
import System.Random

data GameState = GameState
  { gsMainCharacter :: Pos,
    gsEnemies :: [Pos],
    gsStars :: [(Double, Pos)],
    gsBullets :: [Pos],
    gsDragAndDrop :: Maybe DragAndDrop
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

maxStarSize :: Num n => n
maxStarSize = 4

numEnemies :: Int
numEnemies = 10

data TextureId = Enemy | Heart | Plane
  deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

newtype TextureFile = TextureFile FilePath deriving (Eq, Ord, Show)

textureUse :: TextureId -> TextureUse TextureFile
textureUse = \case
  Plane -> TextureUse 1 $ TextureFile "plane.png"
  Enemy -> TextureUse 0.1 $ TextureFile "enemy_red.png"
  Heart -> TextureUse 0.025 $ TextureFile "haskell_love_logo.png"

makeInitialGameState :: Dimensions -> IO GameState
makeInitialGameState Dimensions {width, height} = do
  let randInts = sequence $ replicate 510 randomIO
  let randInts' = sequence $ replicate 510 randomIO
  let randInts'' = sequence $ replicate 510 randomIO
  sizes <- fmap (`mod` maxStarSize) <$> randInts
  xs <- fmap (`mod` double2Int (width + maxStarSize)) <$> randInts'
  ys <- fmap (`mod` double2Int height) <$> randInts''

  pure
    GameState
      { gsMainCharacter = Pos 10 200,
        gsEnemies = mempty,
        gsStars = fmap int2Double sizes `zip` (uncurry Pos <$> fmap int2Double xs `zip` fmap int2Double ys),
        gsBullets = mempty,
        gsDragAndDrop = Nothing
      }

stepGameStatePure :: [Int] -> (TextureId -> Texture) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure pre textures old_gs es event =
  foldl
    (&)
    old_gs
    [ \gs -> dragAndDrop es gs MouseButton'1 (getBulletAreas gs) setBullets (getDragAndDrop gs) setDragAndDrop event,
      \gs -> deleteOnClick es gs MouseButton'2 (getBulletAreas gs) setBullets event,
      \gs -> stepGameStatePure' pre textures gs es event
    ]
  where
    setBullets bullets gs = gs {gsBullets = bullets}
    getBulletAreas gs = textureArea textureUse textures Heart <$> gsBullets gs
    getDragAndDrop gs = gsDragAndDrop gs
    setDragAndDrop v gs = gs {gsDragAndDrop = v}

stepGameStatePure' :: [Int] -> (TextureId -> Texture) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure' randInts (textureArea textureUse -> area) gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    gs
      { gsBullets =
          gsBullets
            <> relativePoss gsMainCharacter [(300, 100), (300, 145), (325, 200), (325, 290), (300, 340), (300, 390)]
      }
  RenderEvent _ ->
    let distancePerSec = 200
        Dimensions {width, height} = esLogicalDimensions
        velocityX = if MovementAction Left' `setMember` esActions then - distancePerSec else if MovementAction Right' `setMember` esActions then distancePerSec else 0
        velocityY = if MovementAction Up `setMember` esActions then - distancePerSec else if MovementAction Down `setMember` esActions then distancePerSec else 0
        bulletVelocity = 300
        bulletStep = esTimePassed * bulletVelocity
        enemyHeight = 50
        enemyWidth = 50
        survivingEnemies =
          flip filter gsEnemies $ \enemyPos ->
            (x enemyPos > - enemyWidth &&)
              $ not
              $ any (area Enemy enemyPos `collidesWith`) (bulletTrajectory =<< gsBullets)
          where
            bulletTrajectory pos = area Heart <$> trajectoryPixels pos esTimePassed 0 bulletVelocity
        newEnemies = survivingEnemies <> (Pos 1100 . modY <$> take numAdded randInts)
          where
            numAdded = numEnemies - length survivingEnemies
            modY = int2Double . flip mod (double2Int $ height - enemyHeight)
        stepStar (size, pos) = (size,) $ updateY (modX . moveY) $ updateX (modX . moveX) pos
          where
            moveX = subtract $ esTimePassed * 5 * (size + 1)
            moveY = subtract $ esTimePassed * 0 * (size + 1)
            modX = flip mod' (width + maxStarSize)
     in gs
          { gsMainCharacter = gsMainCharacter |+| Dimensions (esTimePassed * velocityX) (esTimePassed * velocityY),
            gsEnemies = updateX (subtract $ esTimePassed * 100) <$> newEnemies,
            gsStars = stepStar <$> gsStars,
            gsBullets = filterX (< 1024) gsBullets <&> updateX (+ bulletStep)
          }
  _ -> gs
