{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module LiveCodingDemo.GameState where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (zip)
import My.Prelude
import Playtime

data TextureId = Heart | Spaceship | Enemy deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

data GameState = GameState
  { gsPlayer :: Pos,
    gsStars :: [Pos],
    gsHearts :: [Pos],
    gsEnemies :: [Pos]
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

makeInitialGameState :: Dim -> Int -> GameState
makeInitialGameState dimensions seed =
  let rng = mkStdGen seed
      numStars = 500
      (stars, _) = randomPoss rng numStars dimensions
   in GameState
        { gsPlayer = (10000, 10000),
          gsStars = [], --stars,
          gsHearts = [],
          gsEnemies = []
        }

stepGameStatePure :: Int -> (TextureId -> Dim) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure seed tDim gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    gs
      { gsHearts = gsHearts <> ((gsPlayer +) <$> [(300, 100), (300, 145), (325, 200), (325, 290), (300, 340), (300, 390)])
      }
  RenderEvent _ ->
    let rng = mkStdGen seed
        --isCollision e b = if (tDim Enemy, e) `collidesWith` (tDim Heart, b)  then Just (e,b) else Nothing
        (hitEnemies, hitHearts) = ([], []) -- unzip $ catMaybes $ isCollision <$> gsEnemies <*> gsHearts
        numAddedEnemies = 10 - length gsEnemies
        addedEnemies = [] -- take numNewEnemies $ repeat 1100 `zip` (fst $ randomsNatDouble rng numNewEnemies $ snd esWindowSize)
        movePlayerY pos =
          if
              | Key'W `setMember` esKeysPressed -> pos - dupe esTimePassed * (0, 200)
              | Key'S `setMember` esKeysPressed -> pos + dupe esTimePassed * (0, 200)
              | True -> pos
        movePlayerX pos =
          if
              | Key'A `setMember` esKeysPressed -> pos - dupe esTimePassed * (200, 0)
              | Key'D `setMember` esKeysPressed -> pos + dupe esTimePassed * (200, 0)
              | True -> pos
        newHearts = (gsHearts \\ hitHearts) -- filter ((< fst esWindowSize) . fst) $ (gsHearts \\ hitHearts)
        newEnemies = ((gsEnemies \\ hitEnemies) <> addedEnemies) -- <&> (`mod2` (esWindowSize + tDim Enemy)) . (subtract $ dupe esTimePassed * (200, 0))
     in gs
          { gsPlayer = movePlayerX $ movePlayerY gsPlayer,
            gsStars = gsStars, -- gsStars <&> (`mod2` esWindowSize) . (subtract $ dupe esTimePassed * (50, 0)),
            gsHearts = newHearts, --  <&> (+ dupe esTimePassed * (200, 0)),
            gsEnemies = newEnemies
          }
  _ -> gs
