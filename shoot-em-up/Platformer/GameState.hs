module Platformer.GameState where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Real ((/))
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime.Types

data GameState = GameState
  { gsMainCharacterPosition :: Pos,
    gsEnemyPosition :: Pos,
    gsBullets :: Set Pos
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

gridsize :: Num n => n
gridsize = 12

makeInitialGameState :: Dimensions -> GameState
makeInitialGameState Dimensions {height} =
  GameState
    { gsMainCharacterPosition = Pos 10 (height / 2),
      gsEnemyPosition = Pos 800 (height / 2),
      gsBullets = mempty
    }

stepGameState' :: EngineState -> GameState -> Event -> GameState
stepGameState' EngineState {..} gs@GameState {..} = \case
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
     in gs
          { gsMainCharacterPosition = gsMainCharacterPosition |+| Dimensions (gsTimePassed * velocityX) (gsTimePassed * velocityY),
            gsBullets = map (|+| Dimensions (gsTimePassed * 500) 0) $ setFilter (\(Pos x _) -> x < 1200) gsBullets
          }
  _ -> gs
