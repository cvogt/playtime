module Platformer.GameState where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.List.NonEmpty as NEL (zip)
import GHC.Real ((/))
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime.Geometry
import Playtime.Textures
import Playtime.Types

newtype Board = Board {unBoard :: Map Pos TextureId} deriving newtype (Show, Semigroup, Monoid, NFData)

data GameState = GameState
  { gsCollisions :: Corners (Maybe Area),
    gsVelocityY :: Double,
    gsVelocityX :: Double,
    gsMainCharacterPosition :: Pos,
    gsMainCharacterPositionPrevious :: Pos,
    gsPenetrable :: Board,
    gsRoom :: Board
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

gridsize :: Num n => n
gridsize = 12

makeInitialGameState :: Dimensions -> GameState
makeInitialGameState Dimensions {width} =
  GameState
    { gsCollisions = Corners Nothing Nothing Nothing Nothing,
      gsVelocityY = 0,
      gsVelocityX = 0,
      gsMainCharacterPosition = Pos (width / 2) 0,
      gsMainCharacterPositionPrevious = Pos (width / 2) 0,
      gsPenetrable = Board $ mempty,
      gsRoom =
        Board
          $ mapInsert (Pos 240 188) FloorPlate
          $ mapInsert (Pos 240 176) FloorPlate
          $ mapKeys (uncurry Pos)
          $ mapFromList
          $ concat
          $ take 10
          $ toList
          $ (iterate (+ 12) 200 <&>)
          $ (\r -> take 60 $ toList $ (iterate (+ 12) 0 `NEL.zip` repeat r) `NEL.zip` (repeat FloorPlate))
    }

stepGameState' :: EngineState -> GameState -> Event -> GameState
stepGameState' EngineState {..} gs@GameState {..} = \case
  KeyEvent Key'Space KeyState'Pressed -> gs {gsVelocityY = -220}
  RenderEvent _ ->
    let speedX = 100
        newMainCharacterPosition =
          move
            gsTimePassed
            (Area gsMainCharacterPosition 12)
            gsMainCharacterPositionPrevious
            gsVelocityX
            gsVelocityY
            $ flip Area 12 <$> (keys $ unBoard gsRoom)
     in gs
          { gsMainCharacterPosition = newMainCharacterPosition,
            gsMainCharacterPositionPrevious = gsMainCharacterPosition,
            gsVelocityY =
              if gsVelocityY /= 0 && y gsMainCharacterPosition == y newMainCharacterPosition
                then 0
                else gsVelocityY + 9.81 * gsTimePassed * 55,
            gsVelocityX =
              if Key'A `setMember` gsKeysPressed
                then - speedX
                else
                  if Key'D `setMember` gsKeysPressed
                    then speedX
                    else 0
          }
  _ -> gs

instance FromJSON Board where parseJSON = fmap (Board . mapFromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . mapToList . unBoard
