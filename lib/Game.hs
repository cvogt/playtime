module Game where

import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Float (int2Double)
import GHC.Real ((/), floor, fromIntegral)
import "GLFW-b" Graphics.UI.GLFW as GLFW
import My.Prelude
import SpaceMiner.Textures
import SpaceMiner.Types
import SpaceMiner.Util

makeInitialGameState :: Dimensions -> SystemTime -> GameState
makeInitialGameState Dimensions {width, height} time =
  GameState
    { gsBoard = mempty,
      gsPlacementMode = False,
      gsDeleteMode = False,
      gsKeysPressed = mempty,
      gsMainCharacterPosition = Pos (int2Double width / 2) (int2Double height / 2),
      gsActiveTile = FloorPlate,
      gsExitGame = False,
      gsTimes = [],
      gsFps = 0,
      gsCursorPos = Pos 0 0,
      gsLastPlacement = Pos 0 0,
      gsLastLoopTime = time
    }

maybeExitGameLoop :: Applicative m => GameState -> m (Maybe GameState)
maybeExitGameLoop gs = pure $ if gsExitGame gs then Nothing else Just gs

handleEvent :: GameState -> Event -> GameState
handleEvent = \gs@GameState {..} -> \case
  MouseEvent' (MouseEvent MouseButton'1 MouseButtonState'Pressed _) -> gs {gsPlacementMode = True}
  MouseEvent' (MouseEvent MouseButton'1 MouseButtonState'Released _) -> gs {gsPlacementMode = False}
  MouseEvent' (MouseEvent MouseButton'2 MouseButtonState'Pressed _) -> gs {gsDeleteMode = True}
  MouseEvent' (MouseEvent MouseButton'2 MouseButtonState'Released _) -> gs {gsDeleteMode = False}
  KeyEvent' (KeyEvent Key'Q KeyState'Pressed _) -> gs {gsExitGame = True}
  CursorPosEvent' (CursorPosEvent pos) -> gs {gsCursorPos = pos}
  WindowCloseEvent -> gs {gsExitGame = True}
  KeyEvent' (KeyEvent key KeyState'Pressed _) ->
    gs
      { gsKeysPressed = Set.insert key gsKeysPressed,
        gsActiveTile = case key of
          Key'1 -> FloorPlate
          Key'2 -> TopWall
          _ -> gsActiveTile
      }
  KeyEvent' (KeyEvent key KeyState'Released _) -> gs {gsKeysPressed = Set.delete key gsKeysPressed}
  GameLoopEvent time ->
    let picosecs = timeDiffPico gsLastLoopTime time
        timePassed = int2Double (fromIntegral picosecs) / 1000 / 1000 / 1000 / 1000
        distancePerSec = 100
        halfsec = 500 * 1000 * 1000 * 1000
        d = timePassed * distancePerSec
        Pos x y = gsMainCharacterPosition
        newY = if elem Key'W gsKeysPressed then y - d else if elem Key'S gsKeysPressed then y + d else y
        newX = if elem Key'A gsKeysPressed then x - d else if elem Key'D gsKeysPressed then x + d else x
        gridsize :: Double
        gridsize = 12
        gridify :: Double -> Double
        gridify = (* gridsize) . int2Double . floor . (/ gridsize)
        placement = case gsCursorPos of Pos x' y' -> Pos (gridify x') (gridify y')
     in gs
          { gsLastLoopTime = time,
            gsMainCharacterPosition = Pos newX newY,
            gsTimes = if sum gsTimes > halfsec then [] else picosecs : gsTimes,
            gsFps = if sum gsTimes > halfsec then avg gsTimes else gsFps,
            gsLastPlacement = placement,
            gsBoard = if gsPlacementMode then Map.insert placement gsActiveTile gsBoard else if gsDeleteMode then Map.delete placement gsBoard else gsBoard
          }
  _ -> gs
