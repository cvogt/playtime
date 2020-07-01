module Game where

import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Float (int2Double)
import GHC.Real ((/), floor, fromIntegral)
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import SpaceMiner.Textures
import SpaceMiner.Types
import SpaceMiner.Util

makeInitialGameState :: Dimensions -> SystemTime -> GameState
makeInitialGameState Dimensions {width, height} time =
  GameState
    GenericGameState
      { gsCursorPos = Pos 0 0,
        gsFps = 0,
        gsKeysPressed = mempty,
        gsLastLoopTime = time,
        gsRequestedExitGame = False,
        gsRequestedLoadGame = False,
        gsRequestedResetGame = False,
        gsRequestedSaveGame = False,
        gsTimes = []
      }
    TransientGameState
      { gsDeleteMode = False,
        gsPlacementMode = False,
        gsLastPlacement = Pos 0 0
      }
    PersistentGameState
      { gsActiveTile = FloorPlate,
        gsBoard = mempty,
        gsMainCharacterPosition = Pos (int2Double width / 2) (int2Double height / 2)
      }

appleEventsToGameState :: [Event] -> GameState -> GameState
appleEventsToGameState events gameState =
  foldl
    handleGameEvent
    gameState
      { gsGenericGameState =
          (gsGenericGameState gameState)
            { gsRequestedSaveGame = False,
              gsRequestedLoadGame = False,
              gsRequestedExitGame = False
            }
      }
    events
  where
    handleGameEvent :: GameState -> Event -> GameState
    handleGameEvent (GameState ggs@GenericGameState {..} tgs@TransientGameState {..} pgs@PersistentGameState {..}) event =
      GameState newGeneric newTransient newPersistent
      where
        newGeneric =
          let gs = ggs
           in case event of
                CursorPosEvent' (CursorPosEvent pos) -> gs {gsCursorPos = pos}
                KeyEvent' keyEvent -> case keyEvent of
                  KeyEvent key KeyState'Pressed ModifierKeys {modifierKeysSuper = True} -> case key of
                    Key'L -> gs {gsRequestedLoadGame = True}
                    Key'S -> gs {gsRequestedSaveGame = True}
                    _ -> gs
                  KeyEvent Key'Escape KeyState'Pressed _ -> gs {gsRequestedExitGame = True}
                  KeyEvent key KeyState'Pressed _ ->
                    gs {gsKeysPressed = Set.insert key gsKeysPressed}
                  KeyEvent key KeyState'Released _ ->
                    gs {gsKeysPressed = Set.delete key gsKeysPressed}
                  _ -> gs
                WindowCloseEvent -> gs {gsRequestedExitGame = True, gsRequestedSaveGame = True}
                GameLoopEvent time ->
                  let picosecs = timeDiffPico gsLastLoopTime time
                      halfsec = 500 * 1000 * 1000 * 1000
                   in gs
                        { gsLastLoopTime = time,
                          gsTimes = if sum gsTimes > halfsec then [] else picosecs : gsTimes,
                          gsFps = if sum gsTimes > halfsec then avg gsTimes else gsFps
                        }
                _ -> gs
        newTransient =
          let gs = tgs
           in case event of
                MouseEvent' me -> case me of
                  MouseEvent mb MouseButtonState'Pressed _ -> case mb of
                    MouseButton'1 -> gs {gsPlacementMode = True}
                    MouseButton'2 -> gs {gsDeleteMode = True}
                    _ -> gs
                  MouseEvent mb MouseButtonState'Released _ -> case mb of
                    MouseButton'1 -> gs {gsPlacementMode = False}
                    MouseButton'2 -> gs {gsDeleteMode = False}
                    _ -> gs
                GameLoopEvent _ ->
                  let gridsize :: Double
                      gridsize = 12
                      gridify :: Double -> Double
                      gridify = (* gridsize) . int2Double . floor . (/ gridsize)
                      placement = case gsCursorPos of Pos x' y' -> Pos (gridify x') (gridify y')
                   in gs {gsLastPlacement = placement}
                _ -> gs
        newPersistent =
          let gs = pgs
           in case event of
                KeyEvent' (KeyEvent key KeyState'Pressed _) ->
                  gs
                    { gsActiveTile = case key of
                        Key'1 -> FloorPlate
                        Key'2 -> TopWall
                        _ -> gsActiveTile
                    }
                GameLoopEvent time ->
                  let picosecs = timeDiffPico gsLastLoopTime time
                      timePassed = int2Double (fromIntegral picosecs) / 1000 / 1000 / 1000 / 1000
                      distancePerSec = 100
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
                        { gsMainCharacterPosition = Pos newX newY,
                          gsBoard =
                            if gsPlacementMode
                              then Board $ Map.insert placement gsActiveTile (unBoard gsBoard)
                              else
                                if gsDeleteMode
                                  then Board $ Map.delete placement (unBoard gsBoard)
                                  else gsBoard
                        }
                _ -> gs
