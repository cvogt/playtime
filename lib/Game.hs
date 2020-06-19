{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Game where

import qualified Data.Set as Set
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Clock.TAI
import GHC.Float
import GHC.Real ((%), (/), fromIntegral)
import GLFWHelpers
import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Prelude

data GameState = GameState
  { gsBoard :: [CursorPos],
    gsPlacementMode :: Bool,
    gsKeysPressed :: Set Key,
    gsMainCharacterPosition :: CursorPos,
    gsExitGame :: Bool,
    gsLastLoopTime :: SystemTime,
    gsCursorPos :: CursorPos
  }

initialGameState :: SystemTime -> CursorPos -> GameState
initialGameState = GameState [] False Set.empty (CursorPos (320, 240)) False

handleEvent :: GameState -> InputEvent -> GameState
handleEvent (GameState oldBoard oldPlacementMode oldKeysPressed oldMainCharacterPosition oldExitGame oldLastLoopTime oldCursorPos) input =
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
      newKeysPressed = case input of
        KeyEvent' (KeyEvent key KeyState'Pressed _) -> Set.insert key oldKeysPressed
        KeyEvent' (KeyEvent key KeyState'Released _) -> Set.delete key oldKeysPressed
        _ -> oldKeysPressed
      newExitGame = case input of
        KeyEvent' (KeyEvent Key'Q KeyState'Pressed _) -> True
        _ -> oldExitGame
      newMainCharacterPosition = case input of
        GameLoopEvent time ->
          let picosecs = diffTimeToPicoseconds $ diffAbsoluteTime (systemToTAITime time) (systemToTAITime oldLastLoopTime)
              timePassed = int2Double (fromIntegral picosecs) / 1000 / 1000 / 1000 / 1000
              distancePerSec = 500
              d = timePassed * distancePerSec
              CursorPos (x, y) = oldMainCharacterPosition
              newY = if Set.member Key'W newKeysPressed then y - d else if Set.member Key'S newKeysPressed then y + d else y
              newX = if Set.member Key'A newKeysPressed then x - d else if Set.member Key'D newKeysPressed then x + d else x
           in CursorPos (newX, newY)
        _ -> oldMainCharacterPosition
      newLastLoopTime = case input of
        GameLoopEvent time -> time
        _ -> oldLastLoopTime
   in GameState newBoard newPlacementMode newKeysPressed newMainCharacterPosition newExitGame newLastLoopTime newCursorPos
