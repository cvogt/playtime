module Game where

import qualified Data.Set as Set
import Data.Time.Clock.System
import GHC.Float (int2Double)
import GHC.Real ((/), fromIntegral, round)
import GLFWHelpers
import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Prelude
import SpaceMiner.Util

data GameState = GameState
  { gsBoard :: Set CursorPos,
    gsPlacementMode :: Bool,
    gsKeysPressed :: Set Key,
    gsMainCharacterPosition :: CursorPos,
    gsExitGame :: Bool,
    gsTimes :: [Integer],
    gsFps :: Double,
    gsCursorPos :: CursorPos,
    gsLastLoopTime :: SystemTime
  }

initialGameState :: SystemTime -> GameState
initialGameState = GameState mempty False Set.empty (CursorPos (320, 240)) False [] 0 (CursorPos (0, 0))

handleEvent :: GameState -> InputEvent -> GameState
handleEvent = \gs@GameState {gsBoard, gsPlacementMode, gsKeysPressed, gsMainCharacterPosition, gsLastLoopTime, gsCursorPos, gsFps, gsTimes} -> \case
  MouseEvent' (MouseEvent _ MouseButtonState'Pressed _ cursor) -> gs {gsPlacementMode = True, gsBoard = Set.insert cursor gsBoard}
  MouseEvent' (MouseEvent _ MouseButtonState'Released _ _) -> gs {gsPlacementMode = False}
  CursorPosEvent' (CursorPosEvent pos) -> gs {gsCursorPos = pos}
  KeyEvent' (KeyEvent Key'Q KeyState'Pressed _) -> gs {gsExitGame = True}
  WindowCloseEvent -> gs {gsExitGame = True}
  KeyEvent' (KeyEvent key KeyState'Pressed _) -> gs {gsKeysPressed = Set.insert key gsKeysPressed}
  KeyEvent' (KeyEvent key KeyState'Released _) -> gs {gsKeysPressed = Set.delete key gsKeysPressed}
  GameLoopEvent time ->
    let picosecs = timeDiffPico gsLastLoopTime time
        timePassed = int2Double (fromIntegral picosecs) / 1000 / 1000 / 1000 / 1000
        distancePerSec = 200
        halfsec = 500 * 1000 * 1000 * 1000
        d = round $ timePassed * distancePerSec
        CursorPos (x, y) = gsMainCharacterPosition
        newY = if elem Key'W gsKeysPressed then y - d else if elem Key'S gsKeysPressed then y + d else y
        newX = if elem Key'A gsKeysPressed then x - d else if elem Key'D gsKeysPressed then x + d else x
     in gs
          { gsLastLoopTime = time,
            gsMainCharacterPosition = CursorPos (newX, newY),
            gsTimes = if sum gsTimes > halfsec then [] else picosecs : gsTimes,
            gsFps = if sum gsTimes > halfsec then avg gsTimes else gsFps,
            gsBoard = if gsPlacementMode then Set.insert gsCursorPos gsBoard else gsBoard
          }
  _ -> gs
