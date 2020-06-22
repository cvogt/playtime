module Game where

import qualified Data.Set as Set
import Data.Time.Clock.System
import GHC.Float (int2Double)
import GHC.Real ((/), floor, fromIntegral)
import GLFWHelpers
import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Extra
import My.Prelude
import SpaceMiner.Util

data GameState = GameState
  { gsBoard :: Set CursorPos,
    gsPlacementMode :: Bool,
    gsDeleteMode :: Bool,
    gsKeysPressed :: Set Key,
    gsMainCharacterPosition :: CursorPos,
    gsExitGame :: Bool,
    gsTimes :: [Integer],
    gsFps :: Double,
    gsCursorPos :: CursorPos,
    gsLastPlacement :: CursorPos,
    gsLastLoopTime :: SystemTime
  }

initialGameState :: SystemTime -> GameState
initialGameState = GameState mempty False False Set.empty (CursorPos (320, 240)) False [] 0 (CursorPos (0, 0)) (CursorPos (0, 0))

handleEvent :: GameState -> InputEvent -> GameState
handleEvent = \gs@GameState {gsDeleteMode, gsBoard, gsPlacementMode, gsKeysPressed, gsMainCharacterPosition, gsLastLoopTime, gsCursorPos, gsFps, gsTimes} -> \case
  MouseEvent' (MouseEvent MouseButton'1 MouseButtonState'Pressed _) -> gs {gsPlacementMode = True}
  MouseEvent' (MouseEvent MouseButton'1 MouseButtonState'Released _) -> gs {gsPlacementMode = False}
  MouseEvent' (MouseEvent MouseButton'2 MouseButtonState'Pressed _) -> gs {gsDeleteMode = True}
  MouseEvent' (MouseEvent MouseButton'2 MouseButtonState'Released _) -> gs {gsDeleteMode = False}
  KeyEvent' (KeyEvent Key'Q KeyState'Pressed _) -> gs {gsExitGame = True}
  CursorPosEvent' (CursorPosEvent pos) -> gs {gsCursorPos = pos}
  WindowCloseEvent -> gs {gsExitGame = True}
  KeyEvent' (KeyEvent key KeyState'Pressed _) -> gs {gsKeysPressed = Set.insert key gsKeysPressed}
  KeyEvent' (KeyEvent key KeyState'Released _) -> gs {gsKeysPressed = Set.delete key gsKeysPressed}
  GameLoopEvent time ->
    let picosecs = timeDiffPico gsLastLoopTime time
        timePassed = int2Double (fromIntegral picosecs) / 1000 / 1000 / 1000 / 1000
        distancePerSec = 100
        halfsec = 500 * 1000 * 1000 * 1000
        d = timePassed * distancePerSec
        CursorPos (x, y) = gsMainCharacterPosition
        newY = if elem Key'W gsKeysPressed then y - d else if elem Key'S gsKeysPressed then y + d else y
        newX = if elem Key'A gsKeysPressed then x - d else if elem Key'D gsKeysPressed then x + d else x
        gridsize :: Double
        gridsize = 12
        gridify :: Double -> Double
        gridify = (* gridsize) . int2Double . floor . (/ gridsize)
        placement = CursorPos $ both gridify $ unCursorPos gsCursorPos
     in gs
          { gsLastLoopTime = time,
            gsMainCharacterPosition = CursorPos (newX, newY),
            gsTimes = if sum gsTimes > halfsec then [] else picosecs : gsTimes,
            gsFps = if sum gsTimes > halfsec then avg gsTimes else gsFps,
            gsLastPlacement = placement,
            gsBoard = if gsPlacementMode then Set.insert placement gsBoard else if gsDeleteMode then Set.delete placement gsBoard else gsBoard
          }
  _ -> gs
