module Playtime.EngineConfig where

import My.IO
import My.Prelude
import Playtime.EngineState
import Playtime.Event
import Playtime.Geometry
import Playtime.Texture

data EngineConfig = EngineConfig
  { ecDim :: Dim,
    ecScale :: Double,
    ecVisualize :: EngineState -> IO [Sprite],
    ecStepGameState :: EngineState -> Event -> IO (),
    ecCheckIfContinue :: EngineState -> IO Bool,
    ecGameDebugInfo :: EngineState -> IO [[Char]]
  }
