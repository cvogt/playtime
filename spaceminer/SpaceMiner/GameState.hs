module SpaceMiner.GameState where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import GHC.Float (int2Double)
import GHC.Real (floor)
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime

data TextureId = Inventory | RedResource | TopWall | MainCharacter | FloorPlate
  deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

textures :: TextureId -> (Scale, FilePath)
textures = \case
  Inventory -> (1, "inventory.png")
  RedResource -> (1, "red_resource.png")
  TopWall -> (1, "top_wall.png")
  MainCharacter -> (1, "main_character.png")
  FloorPlate -> (1, "floor_plate.png")

newtype Board = Board {unBoard :: Map Pos TextureId} deriving newtype (Show, Semigroup, Monoid, NFData)

data UIMode = TexturePlacementMode TextureId | TextureMoveMode deriving (Show, Generic, NFData, ToJSON, FromJSON)

data GameState = GameState
  { gsUIMode :: UIMode,
    gsFloor :: Board,
    gsRoom :: Board,
    gsLastPlacement :: Pos,
    gsMainCharacter :: Pos,
    gsMainCharacterPrevious :: Pos,
    gsInventory :: Container,
    gsContainer :: Container,
    gsDragging :: Bool
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

gridsize :: Num n => n
gridsize = 12

makeInitialGameState :: Dim -> GameState
makeInitialGameState dim =
  GameState
    { gsUIMode = TexturePlacementMode FloorPlate,
      gsFloor = mempty,
      gsRoom = mempty,
      gsLastPlacement = 0,
      gsMainCharacter = dim / (2 :: Scale),
      gsMainCharacterPrevious = dim / (2 :: Scale),
      gsInventory =
        Container (150, 10) 3 (5, 8) $
          (take 5 $ repeat $ Just RedResource)
            <> (take 5 $ repeat $ Just MainCharacter)
            <> (take (5 * 6) $ repeat Nothing),
      gsContainer = Container (10, 10) 3 (5, 8) $ take (5 * 8) $ repeat Nothing,
      gsDragging = False
    }

data Container = Container
  { cPos :: Pos,
    cSpacing :: Double,
    cSlots :: (Int, Int),
    cContents :: [Maybe TextureId]
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

dragAndDropInventory ::
  EngineState -> gs -> (gs -> Area) -> (Pos -> gs) -> Event -> gs
dragAndDropInventory EngineState {..} gs get set = \case
  CursorPosEvent _
    | esCursorPosPrevious `isWithin` get gs && MouseButton'1 `setMember` esMousePressed ->
      set $ snd (get gs) + (esCursorPos - esCursorPosPrevious)
  _ -> gs

stepGameStatePure :: (TextureId -> Dim) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure tDim old_gs es event =
  foldl
    (&)
    old_gs {gsDragging = False}
    [ \gs -> dragAndDropInventory es gs (cArea . gsInventory) (\pos -> gs {gsDragging = True, gsInventory = (gsInventory gs) {cPos = pos}}) event, -- (getBulletAreas gs) setBullets (getDragAndDrop gs) setDragAndDrop event,
      \gs -> dragAndDropInventory es gs (cArea . gsContainer) (\pos -> gs {gsDragging = True, gsContainer = (gsContainer gs) {cPos = pos}}) event, -- (getBulletAreas gs) setBullets (getDragAndDrop gs) setDragAndDrop event,
      -- \gs -> deleteOnClick es gs MouseButton'2 (getBulletAreas gs) setBullets event,
      \gs -> stepGameStatePure' tDim gs es event
    ]
  where
    containerSlotSize = 12
    cArea Container {cPos, cSlots = (col, row)} = (dupe containerSlotSize * (int2Double col, int2Double row), cPos)

stepGameStatePure' :: (TextureId -> Dim) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure' tDim gs@GameState {..} EngineState {..} = \case
  CursorPosEvent _ ->
    let placement = bimap (gridify) (gridify) esCursorPos
        gridify = (* gridsize) . int2Double . floor . (/ gridsize)
     in gs
          { gsLastPlacement = placement,
            gsFloor =
              case gsUIMode of
                TexturePlacementMode texture | not gsDragging ->
                  case (`setMember` esMousePressed) of
                    f | f MouseButton'1 && texture == FloorPlate -> Board $ mapInsert placement texture (unBoard gsFloor)
                    f | f MouseButton'2 -> Board $ mapDelete placement (unBoard gsFloor)
                    _ -> gsFloor
                _ -> gsFloor,
            gsRoom =
              case gsUIMode of
                TexturePlacementMode texture | not gsDragging ->
                  case (`setMember` esMousePressed) of
                    f | f MouseButton'1 && texture /= FloorPlate -> Board $ mapInsert placement texture (unBoard gsRoom)
                    f | f MouseButton'2 -> Board $ mapDelete placement (unBoard gsRoom)
                    _ -> gsRoom
                _ -> gsRoom
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
            velocityX = if MovementAction Left' `setMember` esActions then 0 - distancePerSec else if MovementAction Right' `setMember` esActions then 0 + distancePerSec else 0
            velocityY = if MovementAction Up `setMember` esActions then 0 - distancePerSec else if MovementAction Down `setMember` esActions then 0 + distancePerSec else 0
         in gs
              { gsMainCharacter = move esTimePassed (tDim MainCharacter, gsMainCharacter) gsMainCharacterPrevious velocityX velocityY $ (tDim FloorPlate,) <$> (keys $ unBoard gsRoom),
                gsMainCharacterPrevious = gsMainCharacter
              }
  _ -> gs

instance FromJSON Board where parseJSON = fmap (Board . mapFromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . mapToList . unBoard
