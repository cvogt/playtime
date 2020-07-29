module SpaceMiner.GameState where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import GHC.Float (int2Double)
import GHC.Real ((/), floor)
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime

data TextureId = Inventory | RedResource | TopWall | MainCharacter | FloorPlate
  deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

newtype TextureFile = TextureFile FilePath deriving (Eq, Ord, Show)

textureUse :: TextureId -> TextureUse TextureFile
textureUse = \case
  Inventory -> TextureUse 1 $ TextureFile "inventory.png"
  RedResource -> TextureUse 1 $ TextureFile "red_resource.png"
  TopWall -> TextureUse 1 $ TextureFile "top_wall.png"
  MainCharacter -> TextureUse 1 $ TextureFile "main_character.png"
  FloorPlate -> TextureUse 1 $ TextureFile "floor_plate.png"

newtype Board = Board {unBoard :: Map Pos TextureId} deriving newtype (Show, Semigroup, Monoid, NFData)

data UIMode = TexturePlacementMode TextureId | TextureMoveMode deriving (Show, Generic, NFData, ToJSON, FromJSON)

data GameState = GameState
  { gsUIMode :: UIMode,
    gsCollisions :: (Maybe Area, Maybe Area, Maybe Area, Maybe Area),
    gsCandidates :: [Pos],
    gsFloor :: Board,
    gsRoom :: Board,
    gsLastPlacement :: Pos,
    gsMainCharacter :: Pos,
    gsMainCharacterPrevious :: Pos
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

gridsize :: Num n => n
gridsize = 12

makeInitialGameState :: Dimensions -> GameState
makeInitialGameState Dimensions {width, height} =
  GameState
    { gsUIMode = TexturePlacementMode FloorPlate,
      gsCandidates = mempty,
      gsCollisions = (Nothing, Nothing, Nothing, Nothing),
      gsFloor = mempty,
      gsRoom = mempty,
      gsLastPlacement = Pos 0 0,
      gsMainCharacter = Pos (width / 2) (height / 2),
      gsMainCharacterPrevious = Pos (width / 2) (height / 2)
    }

stepGameStatePure :: (TextureId -> Texture) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure (textureArea textureUse -> area) gs@GameState {..} EngineState {..} = \case
  CursorPosEvent _ ->
    let Pos x y = esCursorPos
        gridify :: Double -> Double
        gridify = (* gridsize) . int2Double . floor . (/ gridsize)
        placement = Pos (gridify x) (gridify y)
     in gs
          { gsLastPlacement = placement,
            gsFloor =
              case gsUIMode of
                TexturePlacementMode texture ->
                  case (`setMember` esMousePressed) of
                    f | f MouseButton'1 && texture == FloorPlate -> Board $ mapInsert placement texture (unBoard gsFloor)
                    f | f MouseButton'2 -> Board $ mapDelete placement (unBoard gsFloor)
                    _ -> gsFloor
                TextureMoveMode -> gsFloor,
            gsRoom =
              case gsUIMode of
                TexturePlacementMode texture ->
                  case (`setMember` esMousePressed) of
                    f | f MouseButton'1 && texture /= FloorPlate -> Board $ mapInsert placement texture (unBoard gsRoom)
                    f | f MouseButton'2 -> Board $ mapDelete placement (unBoard gsRoom)
                    _ -> gsRoom
                TextureMoveMode -> gsRoom
          }
  KeyEvent key KeyState'Pressed ->
    gs
      { gsUIMode = case key of
          Key'1 -> TexturePlacementMode FloorPlate
          Key'2 -> TexturePlacementMode TopWall
          Key'3 -> TextureMoveMode
          _ -> gsUIMode
      }
  RenderEvent _ ->
    if OneTimeEffect Reset `setMember` esActions
      then gs {gsFloor = mempty, gsRoom = mempty}
      else
        let distancePerSec = 100
            velocityX = if MovementAction Left' `setMember` esActions then - distancePerSec else if MovementAction Right' `setMember` esActions then distancePerSec else 0
            velocityY = if MovementAction Up `setMember` esActions then - distancePerSec else if MovementAction Down `setMember` esActions then distancePerSec else 0
         in gs
              { gsMainCharacter = move esTimePassed (area MainCharacter gsMainCharacter) gsMainCharacterPrevious velocityX velocityY $ area FloorPlate <$> (keys $ unBoard gsRoom),
                gsMainCharacterPrevious = gsMainCharacter
              }
  _ -> gs

instance FromJSON Board where parseJSON = fmap (Board . mapFromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . mapToList . unBoard
