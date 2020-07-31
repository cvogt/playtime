module LiveCodingDemo.GameState where

import Data.Aeson (FromJSON, ToJSON)
-- import Data.List (zip)
import My.Prelude
import Playtime

data TextureId = Heart | Plane | Enemy deriving (Eq, Ord, Show, Data, Bounded, Enum, Generic, NFData, ToJSON, FromJSON)

data GameState = GameState
  {
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

makeInitialGameState :: Dim -> Int -> GameState
makeInitialGameState _dim (mkStdGen -> _rng) =
  let
   in -- numStars = 510
      -- (poss, rng') = randomPoss rng numStars dim
      -- (sizes, _) = randomRects rng' numStars 4
      GameState
        {
        }

stepGameStatePure :: Int -> (TextureId -> Pos -> Area) -> GameState -> EngineState -> Event -> GameState
stepGameStatePure (mkStdGen -> _rng) _area gs@GameState {} EngineState {..} = \case
  -- KeyEvent Key'Space KeyState'Pressed -> let in gs{ [(300, 100) :: Dim, (300, 145), (325, 200), (325, 290), (300, 340), (300, 390)] }
  -- RenderEvent _ ->
  --   let
  --    in gs
  --         {
  --         }
  _ -> gs
