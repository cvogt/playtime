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
    gsBullets :: [Pos]
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

maxStarSize :: Num n => n
maxStarSize = 4

numEnemies :: Int
numEnemies = 10

newtype TextureId = TextureId [Char] deriving (Eq, Ord, Show)

data TextureUse = TextureUse {tuScale :: Scale, tuId :: TextureId}

plane, enemy, bullet :: TextureUse
plane = TextureUse 1 $ TextureId "plane.png"
enemy = TextureUse 0.1 $ TextureId "enemy_red.png"
bullet = TextureUse 0.025 $ TextureId "haskell_love_logo.png"

textureArea :: (TextureId -> Texture) -> TextureUse -> Pos -> Area
textureArea textures (TextureUse scale (textures -> Texture dim _ _)) pos = Area pos $ scale |*| dim

makeInitialGameState :: Dimensions -> IO GameState
makeInitialGameState Dimensions {width, height} = do
  let randInts = sequence $ replicate 510 randomIO
  sizes <- fmap (`mod` maxStarSize) <$> randInts
  xs <- fmap (`mod` double2Int (width + maxStarSize)) <$> randInts
  ys <- fmap (`mod` double2Int height) <$> randInts

  pure
    GameState
      { gsMainCharacter = Pos 10 200,
        gsEnemies = mempty,
        gsStars = fmap int2Double sizes `zip` (uncurry Pos <$> fmap int2Double xs `zip` fmap int2Double ys),
        gsBullets = mempty
      }

stepGameStatePure :: [Int] -> (TextureId -> Texture) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure randInts (textureArea -> area) gs@GameState {..} EngineState {..} = \case
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
              $ any (area enemy enemyPos `collidesWith`) (bulletTrajectory =<< gsBullets)
          where
            bulletTrajectory pos = area bullet <$> trajectoryPixels pos esTimePassed 0 bulletVelocity
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
