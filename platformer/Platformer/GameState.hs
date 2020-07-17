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
    gsVelocityY :: Double,
    gsVelocityX :: Double,
    gsMainCharacterPosition :: Pos,
    gsRoom :: Board
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

gridsize :: Num n => n
gridsize = 12

makeInitialGameState :: Dimensions -> GameState
makeInitialGameState Dimensions {width} =
  GameState
    { gsCollisions = (Nothing, Nothing, Nothing, Nothing),
      gsVelocityY = 0,
      gsVelocityX = 0,
      gsMainCharacterPosition = Pos (width / 2) 0,
      gsRoom =
        Board
          $ mapInsert (Pos 240 188) FloorPlate
          $ mapInsert (Pos 240 176) FloorPlate
          $ mapKeys (uncurry Pos)
          $ mapFromList
          $ concat
          $ take 10
          $ (iterate (+ 12) 200 <&>)
          $ (\r -> take 60 (iterate (+ 12) 0 `zip` repeat r) `zip` (repeat FloorPlate))
    }

stepGameState' :: EngineState -> GameState -> Event -> GameState
stepGameState' EngineState {..} gs@GameState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    gs
      { gsVelocityY = -220
      }
  RenderEvent time ->
    let picosecs = timeDiffPico gsLastLoopTime time
        timePassed = pico2Double picosecs
        distancePerSec = 100
        charSize = gridsize
        d = timePassed * distancePerSec
        Pos x y = gsMainCharacterPosition
        newY = y + (1 * timePassed * gsVelocityY)
        newX = if MovementAction Left' `setMember` gsActions then x - d else if MovementAction Right' `setMember` gsActions then x + d else x
        newPos = Pos newX newY
        tileArea pos = Area pos charSize
        newArea = tileArea newPos
        tiles = tileArea <$> (keys $ unBoard gsRoom)
        (nw, sw, se, ne) = corners newArea
        nwCollision = find (nw `isWithin`) tiles
        swCollision = find (sw `isWithin`) tiles
        seCollision = find (se `isWithin`) tiles
        neCollision = find (ne `isWithin`) tiles
        collisionsList = [nwCollision, swCollision, seCollision, neCollision]
        newVelocityY = if null $ catMaybes collisionsList then gsVelocityY + (9.81 * timePassed * 55) else 0
        collisions = (nwCollision, swCollision, seCollision, neCollision)
        fixedPos = case collisions of
          (Just _, Just (Area (Pos cX _) (Dimensions cW _)), Nothing, Nothing) -> Pos (cX + cW) newY
          (Nothing, Nothing, Just (Area (Pos cX _) _), Just _) -> Pos (cX - width charSize) newY
          (Nothing, Just _, Just (Area (Pos _ cY) _), Nothing) -> Pos newX (cY - height charSize)
          (Just _, Nothing, Nothing, Just (Area (Pos _ cY) (Dimensions _ cH))) -> Pos newX (cY + cH)
          (Nothing, Nothing, Nothing, Nothing) -> newPos
          _ -> gsMainCharacterPosition
     in gs
          { gsCollisions = collisions,
            gsMainCharacterPosition = fixedPos,
            gsVelocityY = newVelocityY
          }
  _ -> gs

instance FromJSON Board where parseJSON = fmap (Board . mapFromList) . parseJSON

instance ToJSON Board where toJSON = toJSON . mapToList . unBoard