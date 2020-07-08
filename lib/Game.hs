{-# LANGUAGE FlexibleContexts #-}

module Game where

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
        gsInputActions = mempty,
        gsTimes = []
      }
    TransientGameState
      { gsLastPlacement = Pos 0 0,
        gsModes = mempty
      }
    PersistentGameState
      { gsActiveTile = FloorPlate,
        gsBoard = mempty,
        gsMainCharacterPosition = Pos (int2Double width / 2) (int2Double height / 2)
      }

keyBindings :: Map Key [(Set Key, InputAction)]
keyBindings = mapFromList $ groups <&> \l@(h :| _) -> (fst h, first setFromList <$> (join . toList $ snd <$> l))
  where
    groups :: [NonEmpty (Key, [([Key], InputAction)])]
    groups = groupAllWith fst $ join $ keyBindingsRaw <&> (\b@(keys, _) -> (,[b]) <$> keys)
    keyBindingsRaw :: [([Key], InputAction)]
    keyBindingsRaw =
      [ ([Key'LeftSuper, Key'Q], OneTimeAction Exit),
        ([Key'Escape], OneTimeAction Exit),
        ([Key'LeftSuper, Key'L], OneTimeAction Load),
        ([Key'LeftSuper, Key'S], OneTimeAction Save),
        ([Key'LeftSuper, Key'R], OneTimeAction Reset),
        ([Key'W], MovementAction Up),
        ([Key'S], MovementAction Down),
        ([Key'A], MovementAction Left'),
        ([Key'D], MovementAction Right')
      ]

appleEventsToGameState :: [Event] -> GameState -> GameState
appleEventsToGameState events gameState =
  let ggs = gsGenericGameState gameState
   in foldl
        handleGameEvent
        gameState {gsGenericGameState = (gsGenericGameState gameState) {gsInputActions = gsInputActions ggs `difference` (setFromList $ fmap OneTimeAction $ catMaybes $ fmap oneTimeAction $ toList $ gsInputActions ggs)}}
        events
  where
    handleGameEvent :: (Has a GenericGameState, Has a TransientGameState, Has a PersistentGameState) => a -> Event -> GameState
    handleGameEvent a event =
      let ggs@GenericGameState {..} = get a
          tgs@TransientGameState {..} = get a
          pgs@PersistentGameState {..} = get a
          newGeneric =
            let gs = ggs
             in case event of
                  CursorPosEvent pos -> gs {gsCursorPos = pos}
                  KeyEvent key KeyState'Pressed ->
                    let pressed = setInsert key gsKeysPressed
                        matchingBindings = fromMaybe [] $ mapLookup key keyBindings
                        actions = setFromList $ take 1 $ snd <$> filter (null . (`difference` pressed) . fst) matchingBindings
                     in gs {gsKeysPressed = pressed, gsInputActions = gsInputActions `union` actions}
                  KeyEvent key KeyState'Released ->
                    let pressed = setInsert key gsKeysPressed
                        matchingBindings = fromMaybe [] $ mapLookup key keyBindings
                        actions = setFromList $ take 1 $ snd <$> filter (null . (`difference` pressed) . fst) matchingBindings
                     in gs {gsKeysPressed = setDelete key gsKeysPressed, gsInputActions = gsInputActions `difference` actions}
                  WindowCloseEvent -> gs {gsInputActions = setFromList [OneTimeAction Exit, OneTimeAction Save] `union` gsInputActions}
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
                  MouseEvent mb MouseButtonState'Pressed -> case mb of
                    MouseButton'1 -> gs {gsModes = setInsert PlacementMode gsModes}
                    MouseButton'2 -> gs {gsModes = setInsert DeleteMode gsModes}
                    _ -> gs
                  MouseEvent mb MouseButtonState'Released -> case mb of
                    MouseButton'1 -> gs {gsModes = setDelete PlacementMode gsModes}
                    MouseButton'2 -> gs {gsModes = setDelete DeleteMode gsModes}
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
                  KeyEvent key KeyState'Pressed ->
                    gs
                      { gsActiveTile = case key of
                          Key'1 -> FloorPlate
                          Key'2 -> TopWall
                          _ -> gsActiveTile
                      }
                  GameLoopEvent time ->
                    if OneTimeAction Reset `setMember` gsInputActions
                      then gs {gsBoard = mempty}
                      else
                        let picosecs = timeDiffPico gsLastLoopTime time
                            timePassed = int2Double (fromIntegral picosecs) / 1000 / 1000 / 1000 / 1000
                            distancePerSec = 100
                            d = timePassed * distancePerSec
                            Pos x y = gsMainCharacterPosition
                            newY = if MovementAction Up `setMember` gsInputActions then y - d else if MovementAction Down `setMember` gsInputActions then y + d else y
                            newX = if MovementAction Left' `setMember` gsInputActions then x - d else if MovementAction Right' `setMember` gsInputActions then x + d else x
                            gridsize :: Double
                            gridsize = 12
                            gridify :: Double -> Double
                            gridify = (* gridsize) . int2Double . floor . (/ gridsize)
                            placement = case gsCursorPos of Pos x' y' -> Pos (gridify x') (gridify y')
                         in gs
                              { gsMainCharacterPosition = Pos newX newY,
                                gsBoard =
                                  if PlacementMode `setMember` gsModes
                                    then Board $ mapInsert placement gsActiveTile (unBoard gsBoard)
                                    else
                                      if DeleteMode `setMember` gsModes
                                        then Board $ mapDelete placement (unBoard gsBoard)
                                        else gsBoard
                              }
                  _ -> gs
       in GameState newGeneric newTransient newPersistent
