module Platformer.GameState where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.List.NonEmpty as NEL (zip)
import GHC.Real ((/))
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime

main_character, floor_plate :: TextureUse
main_character = TextureUse 1 $ TextureId "main_character.png"
floor_plate = TextureUse 1 $ TextureId "floor_plate.png"

textureArea :: (TextureId -> Texture) -> TextureUse -> Pos -> Area
textureArea textures (TextureUse scale (textures -> Texture dim _ _)) pos = Area pos $ scale |*| dim

data TextureUse = TextureUse {tuScale :: Scale, tuId :: TextureId} deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

newtype TextureId = TextureId [Char]
  deriving (Eq, Ord, Show)
  deriving newtype (NFData, ToJSON, FromJSON)

newtype Board = Board {unBoard :: Map Pos TextureUse} deriving newtype (Show, Semigroup, Monoid, NFData)

data GameState = GameState
  { gsCollisions :: Corners (Maybe Area),
    gsVelocityY :: Double,
    gsVelocityX :: Double,
    gsMainCharacter :: Pos,
    gsMainCharacterPrevious :: Pos,
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
      gsMainCharacter = Pos (width / 2) 0,
      gsMainCharacterPrevious = Pos (width / 2) 0,
      gsPenetrable = Board $ mempty,
      gsRoom =
        Board
          $ mapInsert (Pos 240 188) floor_plate
          $ mapInsert (Pos 240 176) floor_plate
          $ mapKeys (uncurry Pos)
          $ mapFromList
          $ concat
          $ take 10
          $ toList
          $ (iterate (+ 12) 200 <&>)
          $ (\r -> take 60 $ toList $ (iterate (+ 12) 0 `NEL.zip` repeat r) `NEL.zip` (repeat floor_plate))
    }

stepGameStatePure :: (TextureId -> Texture) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure (textureArea -> area) gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'Space KeyState'Pressed -> gs {gsVelocityY = -220}
  RenderEvent _ ->
    let speedX = 100
        newMainCharacter =
          move
            esTimePassed
            (area main_character gsMainCharacter)
            gsMainCharacterPrevious
            gsVelocityX
            gsVelocityY
            $ area floor_plate <$> (keys $ unBoard gsRoom)
     in gs
          { gsMainCharacter = newMainCharacter,
            gsMainCharacterPrevious = gsMainCharacter,
            gsVelocityY =
              if gsVelocityY /= 0 && y gsMainCharacter == y newMainCharacter
                then 0
                else gsVelocityY + 9.81 * esTimePassed * 55,
            gsVelocityX =
              if Key'A `setMember` esKeysPressed
                then - speedX
                else
                  if Key'D `setMember` esKeysPressed
                    then speedX
                    else 0
          }
  _ -> gs

instance FromJSON Board where parseJSON = fmap (Board . mapFromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . mapToList . unBoard
