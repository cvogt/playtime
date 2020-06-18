module Game where

import GLFWHelpers
import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Prelude

data GameState = GameState
  { gsBoard :: [GLFWCursorPosition],
    gsPlacementMode :: Bool
  }

initialGameState :: GameState
initialGameState = GameState [] False

handleEvents :: (Double, Double) -> GameState -> [InputEvent] -> GameState
handleEvents (x, y) = foldl $ handleEvent (x, y)

handleEvent :: (Double, Double) -> GameState -> InputEvent -> GameState
handleEvent (x, y) (GameState oldBoard oldPlacementMode) input =
  let newPlacementMode = case input of
        MouseEvent' (MouseEvent _ MouseButtonState'Pressed _ _) -> True
        MouseEvent' (MouseEvent _ MouseButtonState'Released _ _) -> False
        _ -> oldPlacementMode
      newBoard = case input of
        GameLoopEvent _ -> if newPlacementMode then (GLFWCursorPosition (x, y)) : oldBoard else oldBoard
        MouseEvent' (MouseEvent _ MouseButtonState'Pressed _ cursor) -> cursor : oldBoard
        _ -> oldBoard
   in GameState newBoard newPlacementMode
