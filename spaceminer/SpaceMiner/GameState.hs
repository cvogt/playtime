module SpaceMiner.GameState where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import GHC.Float (int2Double)
import GHC.Real ((/), floor)
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime

newtype Board = Board {unBoard :: Map Pos TextureId} deriving newtype (Show, Semigroup, Monoid, NFData)

data UIMode = TexturePlacementMode TextureId | TextureMoveMode deriving (Show, Generic, NFData, ToJSON, FromJSON)

data GameState = GameState
  { gsUIMode :: UIMode,
    gsCollisions :: (Maybe Area, Maybe Area, Maybe Area, Maybe Area),
    gsCandidates :: [Pos],
    gsFloor :: Board,
    gsRoom :: Board,
    gsLastPlacement :: Pos,
    gsMainCharacterPosition :: Pos,
    gsMainCharacterPositionPrevious :: Pos
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
      gsMainCharacterPosition = Pos (width / 2) (height / 2),
      gsMainCharacterPositionPrevious = Pos (width / 2) (height / 2)
    }

stepGameState' :: GameState -> EngineState -> Event -> GameState
stepGameState' gs@GameState {..} EngineState {..} = \case
  CursorPosEvent _ _ ->
    let Pos x y = gsCursorPos
        gridify :: Double -> Double
        gridify = (* gridsize) . int2Double . floor . (/ gridsize)
        placement = Pos (gridify x) (gridify y)
     in gs
          { gsLastPlacement = placement,
            gsFloor =
              case gsUIMode of
                TexturePlacementMode texture ->
                  case (`setMember` gsMousePressed) of
                    f | f MouseButton'1 && texture == FloorPlate -> Board $ mapInsert placement texture (unBoard gsFloor)
                    f | f MouseButton'2 -> Board $ mapDelete placement (unBoard gsFloor)
                    _ -> gsFloor
                TextureMoveMode -> gsFloor,
            gsRoom =
              case gsUIMode of
                TexturePlacementMode texture ->
                  case (`setMember` gsMousePressed) of
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
    if OneTimeEffect Reset `setMember` gsActions
      then gs {gsFloor = mempty, gsRoom = mempty}
      else
        let distancePerSec = 100
            velocityX = if MovementAction Left' `setMember` gsActions then - distancePerSec else if MovementAction Right' `setMember` gsActions then distancePerSec else 0
            velocityY = if MovementAction Up `setMember` gsActions then - distancePerSec else if MovementAction Down `setMember` gsActions then distancePerSec else 0
         in gs
              { gsMainCharacterPosition = move gsTimePassed (Area gsMainCharacterPosition 12) gsMainCharacterPositionPrevious velocityX velocityY $ flip Area 12 <$> (keys $ unBoard gsRoom),
                gsMainCharacterPositionPrevious = gsMainCharacterPosition
              }
  _ -> gs

instance FromJSON Board where parseJSON = fmap (Board . mapFromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . mapToList . unBoard
