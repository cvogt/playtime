module Game where

import qualified Data.Set as Set
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Clock.TAI
import GHC.Float
import GHC.Real ((/), fromIntegral)
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
handleEvent = \gs@GameState {gsBoard, gsPlacementMode, gsKeysPressed, gsMainCharacterPosition, gsLastLoopTime, gsCursorPos} -> \case
  MouseEvent' (MouseEvent _ MouseButtonState'Pressed _ cursor) -> gs {gsPlacementMode = True, gsBoard = cursor : gsBoard}
  MouseEvent' (MouseEvent _ MouseButtonState'Released _ _) -> gs {gsPlacementMode = False}
  CursorPosEvent' (CursorPosEvent pos) -> gs {gsCursorPos = pos}
  KeyEvent' (KeyEvent Key'Q KeyState'Pressed _) -> gs {gsExitGame = True}
  WindowCloseEvent -> gs {gsExitGame = True}
  KeyEvent' (KeyEvent key KeyState'Pressed _) -> gs {gsKeysPressed = Set.insert key gsKeysPressed}
  KeyEvent' (KeyEvent key KeyState'Released _) -> gs {gsKeysPressed = Set.delete key gsKeysPressed}
  GameLoopEvent time ->
    let picosecs = diffTimeToPicoseconds $ diffAbsoluteTime (systemToTAITime time) (systemToTAITime gsLastLoopTime)
        timePassed = int2Double (fromIntegral picosecs) / 1000 / 1000 / 1000 / 1000
        distancePerSec = 200
        d = timePassed * distancePerSec
        CursorPos (x, y) = gsMainCharacterPosition
        newY = if Set.member Key'W gsKeysPressed then y - d else if Set.member Key'S gsKeysPressed then y + d else y
        newX = if Set.member Key'A gsKeysPressed then x - d else if Set.member Key'D gsKeysPressed then x + d else x
     in gs
          { gsLastLoopTime = time,
            gsMainCharacterPosition = CursorPos (newX, newY),
            gsBoard = if gsPlacementMode then gsCursorPos : gsBoard else gsBoard
          }
  _ -> gs
