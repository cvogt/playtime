module LiveCodingDemo.GameState where

import Data.Aeson (FromJSON, ToJSON)
--import Data.List (zip)
--import GHC.Float (double2Int, int2Double)
--import GHC.Real (mod)
import My.IO
import My.Prelude
--import Playtime.Geometry
import Playtime
import System.Random

data GameState = GameState
  { gsCompileErrors :: Maybe [Char]
  }
  deriving (Show, Generic, NFData, ToJSON, FromJSON)

makeInitialGameState :: Dimensions -> IO GameState
makeInitialGameState Dimensions {} = do
  _random :: [Int] <- sequence $ replicate 500 randomIO
  pure
    GameState
      { gsCompileErrors = Nothing
      }

stepGameStatePure :: [Int] -> EngineState -> GameState -> Event -> GameState
stepGameStatePure _randInts EngineState {..} gs@GameState {..} = \case
  -- KeyEvent Key'Space KeyState'Pressed ->
  --   gs
  --     {
  --     }
  RenderEvent _ ->
    let velocity = 200
        _step = gsTimePassed * velocity
     in gs
  _ -> gs
