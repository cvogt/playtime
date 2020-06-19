module Game where

import GLFWHelpers
import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Prelude

data GameState = GameState
  { gsBoard :: [CursorPos],
    gsPlacementMode :: Bool,
    gsExitGame :: Bool,
    gsCursorPos :: CursorPos
  }

initialGameState :: CursorPos -> GameState
initialGameState = GameState [] False False

handleEvent :: GameState -> InputEvent -> GameState
handleEvent (GameState oldBoard oldPlacementMode oldExitGame oldCursorPos) input =
  let newPlacementMode = case input of
        MouseEvent' (MouseEvent _ MouseButtonState'Pressed _ _) -> True
        MouseEvent' (MouseEvent _ MouseButtonState'Released _ _) -> False
        _ -> oldPlacementMode
      newBoard = case input of
        GameLoopEvent _ -> if newPlacementMode then oldCursorPos : oldBoard else oldBoard
        MouseEvent' (MouseEvent _ MouseButtonState'Pressed _ cursor) -> cursor : oldBoard
        _ -> oldBoard
      newCursorPos = case input of
        CursorPosEvent' (CursorPosEvent pos) -> pos
        _ -> oldCursorPos
      newExitGame = case input of
        KeyEvent' (KeyEvent Key'Q KeyState'Pressed _) -> True
        _ -> oldExitGame
   in GameState newBoard newPlacementMode newExitGame newCursorPos
