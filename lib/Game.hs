module Game where

import My.Prelude

data GameState = GameState
  { gsBoard :: [(Double, Double)],
    gsPlacementMode :: Bool
  }

initialGameState :: GameState
initialGameState = GameState [] False
