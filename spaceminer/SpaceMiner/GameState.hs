module SpaceMiner.GameState where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import GHC.Float (int2Double)
import GHC.Real ((/), floor)
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime

inventory, red_resource, top_wall, main_character, floor_plate :: TextureUse
inventory = TextureUse 1 $ TextureId "inventory.png"
red_resource = TextureUse 1 $ TextureId "red_resource.png"
top_wall = TextureUse 1 $ TextureId "top_wall.png"
main_character = TextureUse 1 $ TextureId "main_character.png"
floor_plate = TextureUse 1 $ TextureId "floor_plate.png"

textureArea :: (TextureId -> Texture) -> TextureUse -> Pos -> Area
textureArea textures (TextureUse scale (textures -> Texture dim _ _)) pos = Area pos $ scale |*| dim

data TextureUse = TextureUse {tuScale :: Scale, tuId :: TextureId} deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

newtype TextureId = TextureId [Char]
  deriving (Eq, Ord, Show)
  deriving newtype (NFData, ToJSON, FromJSON)

newtype Board = Board {unBoard :: Map Pos TextureUse} deriving newtype (Show, Semigroup, Monoid, NFData)

data UIMode = TexturePlacementMode TextureUse | TextureMoveMode deriving (Show, Generic, NFData, ToJSON, FromJSON)

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
    { gsUIMode = TexturePlacementMode floor_plate,
      gsCandidates = mempty,
      gsCollisions = (Nothing, Nothing, Nothing, Nothing),
      gsFloor = mempty,
      gsRoom = mempty,
      gsLastPlacement = Pos 0 0,
      gsMainCharacter = Pos (width / 2) (height / 2),
      gsMainCharacterPrevious = Pos (width / 2) (height / 2)
    }

--   foldl (.) id (flap [applyToEngineState, applyToGameState] event')

stepGameStatePure :: (TextureId -> Texture) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure (textureArea -> area) gs@GameState {..} EngineState {..} = \case
  CursorPosEvent _ _ ->
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
                    f | f MouseButton'1 && texture == floor_plate -> Board $ mapInsert placement texture (unBoard gsFloor)
                    f | f MouseButton'2 -> Board $ mapDelete placement (unBoard gsFloor)
                    _ -> gsFloor
                TextureMoveMode -> gsFloor,
            gsRoom =
              case gsUIMode of
                TexturePlacementMode texture ->
                  case (`setMember` esMousePressed) of
                    f | f MouseButton'1 && texture /= floor_plate -> Board $ mapInsert placement texture (unBoard gsRoom)
                    f | f MouseButton'2 -> Board $ mapDelete placement (unBoard gsRoom)
                    _ -> gsRoom
                TextureMoveMode -> gsRoom
          }
  KeyEvent key KeyState'Pressed ->
    gs
      { gsUIMode = case key of
          Key'1 -> TexturePlacementMode floor_plate
          Key'2 -> TexturePlacementMode top_wall
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
              { gsMainCharacter = move esTimePassed (area main_character gsMainCharacter) gsMainCharacterPrevious velocityX velocityY $ area floor_plate <$> (keys $ unBoard gsRoom),
                gsMainCharacterPrevious = gsMainCharacter
              }
  _ -> gs

instance FromJSON Board where parseJSON = fmap (Board . mapFromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . mapToList . unBoard
