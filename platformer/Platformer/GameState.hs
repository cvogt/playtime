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

data GameState = GameState
  { gsCollisions :: Corners (Maybe Area),
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
    { gsCollisions = Corners Nothing Nothing Nothing Nothing,
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
  KeyEvent Key'Space KeyState'Pressed -> gs {gsVelocityY = -220}
  RenderEvent time ->
    let timePassed = pico2Double $ timeDiffPico gsLastLoopTime time
        charSize = gridsize
        newPosCandidate =
          Pos
            (x + (timePassed * gsVelocityX))
            (y + (timePassed * gsVelocityY))
          where
            Pos {x, y} = gsMainCharacterPosition
        collisions :: Corners (Maybe Area)
        collisions = corners newArea <&> \corner -> find (corner `isWithin`) tiles
          where
            tileArea pos = Area pos charSize
            tiles = tileArea <$> (keys $ unBoard gsRoom)
            newArea = tileArea newPosCandidate
        newPosFinal =
          -- Pos finalX finalY
          let Corners {nw, sw, se, ne} = collisions
              Pos newX newY = newPosCandidate
           in case (nw, sw, se, ne) of
                (Just _, Just (Area (Pos cX _) (Dimensions cW _)), Nothing, Nothing) -> Pos (cX + cW) newY
                (Nothing, Nothing, Just (Area (Pos cX _) _), Just _) -> Pos (cX - width charSize) newY
                (Nothing, Just _, Just (Area (Pos _ cY) _), Nothing) -> Pos newX (cY - height charSize)
                (Just _, Nothing, Nothing, Just (Area (Pos _ cY) (Dimensions _ cH))) -> Pos newX (cY + cH)
                (Nothing, Nothing, Nothing, Nothing) -> newPosCandidate
                _ -> gsMainCharacterPosition
        newVelocityY =
          let hasCollisions = not . null . catMaybes $ toList collisions
           in if hasCollisions then 0 else gsVelocityY + (9.81 * timePassed * 55)
        speedX = 100
     in gs
          { gsCollisions = collisions,
            gsMainCharacterPosition = newPosFinal,
            gsVelocityY = newVelocityY,
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
