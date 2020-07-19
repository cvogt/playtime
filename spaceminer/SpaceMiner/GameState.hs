module SpaceMiner.GameState where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import GHC.Float (int2Double)
import GHC.Real ((/), floor, fromIntegral)
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime.Textures
import Playtime.Types
import Playtime.Util
import Data.List (zip)

newtype Board = Board {unBoard :: Map Pos TextureId} deriving newtype (Show, Semigroup, Monoid, NFData)

data UIMode = TexturePlacementMode TextureId | TextureMoveMode deriving (Show, Generic, NFData, ToJSON, FromJSON)

data GameState = GameState
  { gsUIMode :: UIMode,
    gsCollisions :: (Maybe Area, Maybe Area, Maybe Area, Maybe Area),
    gsFloor :: Board,
    gsRoom :: Board,
    gsLastPlacement :: Pos,
    gsMainCharacterPosition :: Pos
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

gridsize :: Num n => n
gridsize = 12

makeInitialGameState :: Dimensions -> GameState
makeInitialGameState Dimensions {width, height} =
  GameState
    { gsUIMode = TexturePlacementMode FloorPlate,
      gsCollisions = (Nothing, Nothing, Nothing, Nothing),
      gsFloor = mempty,
      gsRoom = mempty,
      gsLastPlacement = Pos 0 0,
      gsMainCharacterPosition = Pos (width / 2) (height / 2)
    }

stepGameState' :: EngineState -> GameState -> Event -> GameState
stepGameState' EngineState {..} gs@GameState {..} = \case
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
  RenderEvent time ->
    if OneTimeEffect Reset `setMember` gsActions
      then gs {gsFloor = mempty, gsRoom = mempty}
      else
        let picosecs = timeDiffPico gsLastLoopTime time
            timePassed = int2Double (fromIntegral picosecs) / 1000 / 1000 / 1000 / 1000
            distancePerSec = 100
            charSize = gridsize
            d = timePassed * distancePerSec
            newY = if MovementAction Up `setMember` gsActions then y - d else if MovementAction Down `setMember` gsActions then y + d else y
            newX = if MovementAction Left' `setMember` gsActions then x - d else if MovementAction Right' `setMember` gsActions then x + d else x
            Pos {x, y} = gsMainCharacterPosition
            deltaX = newX - x
            deltaY = newY - y
            (stepX, _stepY) = if deltaX > deltaY then (1, deltaY / deltaX) else (deltaX / deltaY, 1)
            candidatesX = [x + deltaX] -- fmap (x +) . takeWhile (<= deltaX) $ iterate (+ stepX) 0
            candidatesY = [y + deltaY] -- fmap (y +) . takeWhile (<= deltaY) $ iterate (+ stepY) 0
            candidates = uncurry Pos <$> zip candidatesX candidatesY
            tileArea pos = Area pos charSize
            tiles = tileArea <$> (keys $ unBoard gsRoom)
            unobstructedPath = flip takeWhile candidates $ \c -> not $ any (Area c charSize `collidesWith`) tiles
            newPosFinal@(Pos _ _) = case fromMaybe gsMainCharacterPosition $ lastMay unobstructedPath of
              c@(Pos cx cy) ->
                if any ((Area (Pos (cx + stepX) cy) charSize) `collidesWith`) tiles
                  then
                    let remainingCandidatesY = drop (length unobstructedPath) candidatesY
                        remainingCandidates = uncurry Pos <$> zip (repeat cx) remainingCandidatesY
                        remainingUnobstructedPath = flip takeWhile remainingCandidates $ \v -> any (Area v charSize `collidesWith`) tiles
                     in fromMaybe c $ lastMay remainingUnobstructedPath
                  else
                    let remainingCandidatesX = drop (length unobstructedPath) candidatesX
                        remainingCandidates = uncurry Pos <$> zip remainingCandidatesX (repeat cy)
                        remainingUnobstructedPath = flip takeWhile remainingCandidates $ \v -> any (Area v charSize `collidesWith`) tiles
                     in fromMaybe c $ lastMay remainingUnobstructedPath
         in gs {gsMainCharacterPosition = newPosFinal}
  _ -> gs

instance FromJSON Board where parseJSON = fmap (Board . mapFromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . mapToList . unBoard
