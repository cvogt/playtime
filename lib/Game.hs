module Game where

import Data.List (filter, reverse)
import GLFWHelpers
import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Prelude

data GameState = GameState
  { gsBoard :: [(Double, Double)],
    gsPlacementMode :: Bool
  }

initialGameState :: GameState
initialGameState = GameState [] False

handleEvents :: (Double, Double) -> GameState -> CapturedInput -> GameState
handleEvents (x, y) gameState capturedInputs =
  let newPlacementMode = case fmap meButtonState . headMay $ cieMouse capturedInputs of
        Just MouseButtonState'Pressed -> True
        Just MouseButtonState'Released -> False
        Nothing -> gsPlacementMode gameState
      clicks = unGLFWCursorPosition . meCursorPosition <$> (reverse $ filter ((MouseButtonState'Pressed ==) . meButtonState) $ cieMouse capturedInputs)
      newBoard = gsBoard gameState <> clicks <> (if newPlacementMode then [(x, y)] else [])
   in gameState {gsBoard = newBoard, gsPlacementMode = newPlacementMode}
