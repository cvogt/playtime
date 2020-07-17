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
        Pos {x, y} = gsMainCharacterPosition
        deltaX = timePassed * gsVelocityX
        deltaY = timePassed * gsVelocityY
        (stepX, _stepY) = if deltaX > deltaY then (1, deltaY / deltaX) else (deltaX / deltaY, 1)
        candidatesX = [x + deltaX] -- fmap (x +) . takeWhile (<= deltaX) $ iterate (+ stepX) 0
        candidatesY = [y + deltaY] -- fmap (y +) . takeWhile (<= deltaY) $ iterate (+ stepY) 0
        candidates = uncurry Pos <$> zip candidatesX candidatesY
        tileArea pos = Area pos charSize
        tiles = tileArea <$> (keys $ unBoard gsRoom)
        unobstructedPath = flip takeWhile candidates $ \c -> not $ any (Area c charSize `collidesWith`) tiles
        newPosFinal@(Pos _ newY) = case fromMaybe gsMainCharacterPosition $ lastMay unobstructedPath of
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
        -- collisions :: Pos -> Corners (Maybe Area)
        -- collisions newPosCandidate = corners newArea <&> \corner -> find (corner `isWithin`) tiles
        --   where
        --     tileArea pos = Area pos charSize
        --     newArea = tileArea newPosCandidate

        -- newPosFinal =
        --   -- Pos finalX finalY
        --   let Corners {nw, sw, se, ne} = collisions
        --       Pos newX newY = newPosCandidate
        --    in case (nw, sw, se, ne) of
        --         (Just _, Just (Area (Pos cX _) (Dimensions cW _)), Nothing, Nothing) -> Pos (cX + cW) newY
        --         (Nothing, Nothing, Just (Area (Pos cX _) _), Just _) -> Pos (cX - width charSize) newY
        --         (Nothing, Just _, Just (Area (Pos _ cY) _), Nothing) -> Pos newX (cY - height charSize)
        --         (Just _, Nothing, Nothing, Just (Area (Pos _ cY) (Dimensions _ cH))) -> Pos newX (cY + cH)
        --         (Nothing, Nothing, Nothing, Nothing) -> newPosCandidate
        --         _ -> gsMainCharacterPosition
        -- finalX = case (catMaybes [nw, sw], catMaybes [ne, se]) of
        --   ([Area (Pos cX _) (Dimensions cW _)], []) -> cX + cW
        --   ([], [Area (Pos cX _) _]) -> cX - width charSize
        --   _ -> x newPosCandidate
        -- finalY = case (catMaybes [nw, ne], catMaybes [sw, se]) of
        --   ([Area (Pos _ cY) (Dimensions _ cH)], []) -> cY + cH
        --   ([], [Area (Pos _ cY) _]) -> cY - height charSize
        --   _ -> y newPosCandidate
        newVelocityY = if newY == y && gsVelocityY /= 0 then gsVelocityY else gsVelocityY + (9.81 * timePassed * 55)
        speedX = 100
     in gs
          { --gsCollisions = collisions,
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
