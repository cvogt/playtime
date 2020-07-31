{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{- ORMOLU_DISABLE -}

module LiveCodingDemo.GameState where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (zip)
import My.Prelude
import Playtime

data TextureId = Heart | Plane | Enemy deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

data GameState = GameState
  { gsPlayer :: Pos,
    gsStars :: [Pos],
    gsHearts :: [Pos],
    gsEnemies :: [Pos]
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

makeInitialGameState :: Dim -> Int -> GameState
makeInitialGameState windowDimensions seed =
  let rng = mkStdGen seed
      numStars = 500
      (stars, _) = randomPoss rng numStars windowDimensions
  in GameState
        { gsPlayer = (100,100),
          gsStars = stars
          , gsHearts = []
          , gsEnemies = []
        }

stepGameStatePure :: Int -> (TextureId -> Dim) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure seed tDim gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    gs{
      gsHearts = gsHearts <> ((gsPlayer +) <$> [(300, 100), (300, 145), (325, 200), (325, 290), (300, 340), (300, 390)])
    }
  RenderEvent _ ->
    let
      rng = mkStdGen seed
      (hitEnemies, hitHearts) = unzip $ catMaybes $ (\e b ->if (tDim Enemy, e) `collidesWith` (tDim Heart, b)  then Just (e,b) else Nothing) <$> gsEnemies <*> gsHearts
      totalEnemies = 10 - length gsEnemies
      newEnemies = take totalEnemies $ repeat (1024 + (fst $ tDim Enemy)) `zip` (fst $ randomsNatDouble rng totalEnemies $ snd esWindowDimensions)
      movePlayerY pos =
        if
           | Key'W `setMember` esKeysPressed -> pos + dupe esTimePassed * (0,-200)
           | Key'S `setMember` esKeysPressed -> pos + dupe esTimePassed * (0,200)
           | True -> pos
      movePlayerX pos =
        if
           | Key'A `setMember` esKeysPressed -> pos + dupe esTimePassed * (-200,0)
           | Key'D `setMember` esKeysPressed -> pos + dupe esTimePassed * (200,0)
           | True -> pos
     in gs
          { gsPlayer = movePlayerX $ movePlayerY gsPlayer
          , gsStars = gsStars <&>  (`mod2` esWindowDimensions) . (subtract $ dupe esTimePassed * (50,0))
          , gsHearts = (gsHearts \\ hitHearts) <&> (+ dupe esTimePassed * (200,0))
          , gsEnemies = ((gsEnemies \\ hitEnemies) <> newEnemies) <&> (`mod2` (esWindowDimensions + tDim Enemy)) . (subtract $ dupe esTimePassed * (200,0))
          }
  _ -> gs

