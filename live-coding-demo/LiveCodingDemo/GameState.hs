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

makeInitialGameState :: (Relative X, Relative Y) -> Int -> GameState
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
stepGameStatePure seed tdim gs@GameState {..} EngineState {..} = \case
  KeyEvent Key'Space KeyState'Pressed ->
    gs{
      gsHearts = gsHearts <> ((gsPlayer |+) <$> [(300, 100) :: Dim, (300, 145), (325, 200), (325, 290), (300, 340), (300, 390)])
    }
  RenderEvent _ ->
    let
      rng = mkStdGen seed
      (hitEnemies, hitHearts) = unzip $ catMaybes $ (\e b ->if (tdim Enemy, e) `collidesWith` (tdim Heart, b)  then Just (e,b) else Nothing) <$> gsEnemies <*> gsHearts
      totalEnemies = 10 - length gsEnemies
      newEnemies = take totalEnemies $ repeat ((1024 :: Absolute X) |+ tdim Enemy) `zip` (fst $ randomsAbsoluteY rng totalEnemies (snd esWindowDimensions))
      movePlayerY pos =
        if
           | Key'W `setMember` esKeysPressed -> pos |- esTimePassed *| (200 :: Relative Y)
           | Key'S `setMember` esKeysPressed -> pos |+ esTimePassed *| (200 :: Relative Y)
           | True -> pos
      movePlayerX pos =
        if
           | Key'A `setMember` esKeysPressed -> pos |- esTimePassed *| (200 :: Relative X)
           | Key'D `setMember` esKeysPressed -> pos |+ esTimePassed *| (200 :: Relative X)
           | True -> pos
     in gs
          { gsPlayer = (200,200) -- movePlayerX $ movePlayerY gsPlayer
          , gsStars = gsStars <&>  (|% esWindowDimensions) . (|- esTimePassed *| (50 :: Relative X))
          , gsHearts = (gsHearts \\ hitHearts) <&> (|+ esTimePassed *| (200 :: Relative X))
          , gsEnemies = ((gsEnemies \\ hitEnemies) <> newEnemies) <&> (|% (esWindowDimensions |+ tdim Enemy)) . (|- esTimePassed *| (200 :: Relative X))
          }
  _ -> gs
