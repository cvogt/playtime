module ShootEmUp.GameState where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Float (int2Double)
import GHC.Real ((/), mod)
import "GLFW-b" Graphics.UI.GLFW
import My.Extra
import My.Prelude
import Playtime.Geometry
import Playtime.Types

data GameState = GameState
  { gsMainCharacterPosition :: Pos,
    gsEnemies :: Set Pos,
    gsBullets :: Set Pos
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

gridsize :: Num n => n
gridsize = 12

makeInitialGameState :: Dimensions -> GameState
makeInitialGameState Dimensions {height} =
  GameState
    { gsMainCharacterPosition = Pos 10 (height / 2),
      gsEnemies = mempty,
      gsBullets = mempty
    }

stepGameState' :: StdGen -> EngineState -> GameState -> Event -> GameState
stepGameState' rng EngineState {..} gs@GameState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    gs
      { gsBullets =
          gsBullets
            `union` setFromList
              [ gsMainCharacterPosition {x = 300 + x gsMainCharacterPosition, y = y gsMainCharacterPosition + 100},
                gsMainCharacterPosition {x = 300 + x gsMainCharacterPosition, y = y gsMainCharacterPosition + 145},
                gsMainCharacterPosition {x = 325 + x gsMainCharacterPosition, y = y gsMainCharacterPosition + 200},
                gsMainCharacterPosition {x = 325 + x gsMainCharacterPosition, y = y gsMainCharacterPosition + 290},
                gsMainCharacterPosition {x = 300 + x gsMainCharacterPosition, y = y gsMainCharacterPosition + 340},
                gsMainCharacterPosition {x = 300 + x gsMainCharacterPosition, y = y gsMainCharacterPosition + 390}
              ]
      }
  RenderEvent _ ->
    let distancePerSec = 200
        velocityX = if MovementAction Left' `setMember` gsActions then - distancePerSec else if MovementAction Right' `setMember` gsActions then distancePerSec else 0
        velocityY = if MovementAction Up `setMember` gsActions then - distancePerSec else if MovementAction Down `setMember` gsActions then distancePerSec else 0
        bulletVelocity = 500
        survivingEnemies =
          flip setFilter gsEnemies $ \enemyPos ->
            flip all gsBullets $
              \bulletPos -> isNothing $ find (Area bulletPos 3 `collidesWith`) $ flip Area 12 <$> trajectoryPixels enemyPos gsTimePassed 0 bulletVelocity
        newEnemies = (survivingEnemies <>) . setFromList . (Pos 1024 . (int2Double . flip mod 756) <$>) $ take (10 - length survivingEnemies) . toList $ unfoldr (second Just . next) rng
     in gs
          { gsMainCharacterPosition = gsMainCharacterPosition |+| Dimensions (gsTimePassed * velocityX) (gsTimePassed * velocityY),
            gsEnemies = map (|+| Dimensions (- gsTimePassed * 50) 0) $ setFilter (\(Pos x _) -> x > -50) newEnemies,
            gsBullets = map (|+| Dimensions (gsTimePassed * bulletVelocity) 0) $ setFilter (\(Pos x _) -> x < 1200) gsBullets
          }
  _ -> gs
