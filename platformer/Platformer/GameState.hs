module Platformer.GameState where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.List (zip)
import GHC.Real ((/))
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime.Textures
import Playtime.Types
import Playtime.Util

newtype Board = Board {unBoard :: Map Pos TextureId} deriving newtype (Show, Semigroup, Monoid, NFData)

data UIMode = TexturePlacementMode TextureId | TextureMoveMode deriving (Show, Generic, NFData, ToJSON, FromJSON)

data GameState = GameState
  { gsCollisions :: (Maybe Area, Maybe Area, Maybe Area, Maybe Area),
    gsFloor :: Board,
    gsRoom :: Board,
    gsJumped :: Maybe SystemTime,
    gsMainCharacterPosition :: Pos
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

gridsize :: Num n => n
gridsize = 12

makeInitialGameState :: Dimensions -> GameState
makeInitialGameState Dimensions {width} =
  GameState
    { gsJumped = Nothing,
      gsCollisions = (Nothing, Nothing, Nothing, Nothing),
      gsFloor = mempty,
      gsRoom =
        Board
          $ mapKeys (uncurry Pos)
          $ mapFromList
          $ concat
          $ take 10
          $ (iterate (+ 12) 200 <&>)
          $ (\r -> take 60 (iterate (+ 12) 0 `zip` repeat r) `zip` (repeat FloorPlate)),
      gsMainCharacterPosition = Pos (width / 2) 0
    }

stepGameState' :: EngineState -> GameState -> Event -> GameState
stepGameState' EngineState {..} gs@GameState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    gs
      { gsJumped = Just gsLastLoopTime
      }
  RenderEvent time ->
    if OneTimeEffect Reset `setMember` gsActions
      then gs {gsFloor = mempty, gsRoom = mempty}
      else
        let picosecs = timeDiffPico gsLastLoopTime time
            secondsSince since = pico2Double $ timeDiffPico gsLastLoopTime since
            timePassed = pico2Double picosecs
            distancePerSec = 100
            charSize = gridsize
            d = timePassed * distancePerSec
            Pos x y = gsMainCharacterPosition
            newY = case gsJumped of
              Just (abs . secondsSince -> since)
                | since < 1 ->
                  let f v = 40 * (((v * 2) -1) ** 2) - 1
                   in y + f (since) - f (since - timePassed)
              _ -> y + (timePassed * 120)
            newX = if MovementAction Left' `setMember` gsActions then x - d else if MovementAction Right' `setMember` gsActions then x + d else x
            newPos = Pos newX newY
            tileArea pos = Area pos charSize
            newArea = tileArea newPos
            collisions = filter (newArea `collidesWith`) $ tileArea <$> (keys $ unBoard gsRoom)
            (nw, sw, se, ne) = corners newArea
            nwCollision = find (nw `isWithin`) collisions
            swCollision = find (sw `isWithin`) collisions
            seCollision = find (se `isWithin`) collisions
            neCollision = find (ne `isWithin`) collisions
            fixedPos = case (nwCollision, swCollision, seCollision, neCollision) of
              (Just _, Just (Area (Pos cX _) (Dimensions cW _)), Nothing, Nothing) -> Pos (cX + cW) newY
              (Nothing, Nothing, Just (Area (Pos cX _) _), Just _) -> Pos (cX - width charSize) newY
              (Nothing, Just _, Just (Area (Pos _ cY) _), Nothing) -> Pos newX (cY - height charSize)
              (Just _, Nothing, Nothing, Just (Area (Pos _ cY) (Dimensions _ cH))) -> Pos newX (cY + cH)
              (Nothing, Nothing, Nothing, Nothing) -> newPos
              _ -> gsMainCharacterPosition
         in gs {gsCollisions = (nwCollision, swCollision, seCollision, neCollision), gsMainCharacterPosition = fixedPos}
  _ -> gs

instance FromJSON Board where parseJSON = fmap (Board . mapFromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . mapToList . unBoard
