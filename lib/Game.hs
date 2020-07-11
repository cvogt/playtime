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
        gsMousePressed = mempty,
        gsLastLoopTime = time,
        gsActions = mempty,
        gsTimes = []
      }
    TransientGameState {gsLastPlacement = Pos 0 0}
    PersistentGameState
      { gsUIMode = TexturePlacementMode FloorPlate,
        gsBoard = mempty,
        gsMainCharacterPosition = Pos (width / 2) (height / 2)
      }

keyBindings :: Map Key [(Set Key, Action)]
keyBindings = mapFromList $ groups <&> \l@(h :| _) -> (fst h, first setFromList <$> (join . toList $ snd <$> l))
  where
    groups :: [NonEmpty (Key, [([Key], Action)])]
    groups = groupAllWith fst $ join $ keyBindingsRaw <&> (\b@(keys, _) -> (,[b]) <$> keys)
    keyBindingsRaw :: [([Key], Action)]
    keyBindingsRaw =
      [ ([Key'LeftSuper, Key'Q], Exit),
        ([Key'Escape], Exit),
        ([Key'LeftSuper, Key'L], OneTimeEffect Load),
        ([Key'LeftSuper, Key'S], OneTimeEffect Save),
        ([Key'LeftSuper, Key'R], OneTimeEffect Reset),
        ([Key'W], MovementAction Up),
        ([Key'S], MovementAction Down),
        ([Key'A], MovementAction Left'),
        ([Key'D], MovementAction Right')
      ]

applyEventToGameState :: Event -> GameState -> GameState
applyEventToGameState event' gameState =
  foldl (.) id (flap [applyToGenericGameState, applyToTransientGameState, applyToPersistentGameState] event')
    $ update gameState
    $ \ggs ->
      ggs
        { gsActions =
            gsActions ggs
              `difference` (setFromList $ fmap OneTimeEffect $ catMaybes $ fmap oneTimeEffectMay $ toList $ gsActions ggs)
        }
  where
    applyToGenericGameState :: (Has a GenericGameState) => Event -> a -> a
    applyToGenericGameState event a =
      update a $ \(gs@GenericGameState {..}) -> case event of
        CursorPosEvent pos -> gs {gsCursorPos = pos}
        KeyEvent key KeyState'Pressed ->
          let pressed = setInsert key gsKeysPressed
              matchingBindings = fromMaybe [] $ mapLookup key keyBindings
              actions = setFromList $ take 1 $ snd <$> filter (null . (`difference` pressed) . fst) matchingBindings
           in gs {gsKeysPressed = pressed, gsActions = gsActions `union` actions}
        KeyEvent key KeyState'Released ->
          let pressed = setInsert key gsKeysPressed
              matchingBindings = fromMaybe [] $ mapLookup key keyBindings
              actions = setFromList $ take 1 $ snd <$> filter (null . (`difference` pressed) . fst) matchingBindings
           in gs {gsKeysPressed = setDelete key gsKeysPressed, gsActions = gsActions `difference` actions}
        MouseEvent mb MouseButtonState'Pressed -> gs {gsMousePressed = setInsert mb gsMousePressed}
        MouseEvent mb MouseButtonState'Released -> gs {gsMousePressed = setDelete mb gsMousePressed}
        WindowCloseEvent -> gs {gsActions = setFromList [Exit, OneTimeEffect Save] `union` gsActions}
        RenderEvent time ->
          let picosecs = timeDiffPico gsLastLoopTime time
              halfsec = 500 * 1000 * 1000 * 1000
           in gs
                { gsLastLoopTime = time,
                  gsTimes = if sum gsTimes > halfsec then [] else picosecs : gsTimes,
                  gsFps = if sum gsTimes > halfsec then avg gsTimes else gsFps
                }
        _ -> gs
    applyToTransientGameState :: (Has a GenericGameState, Has a TransientGameState) => Event -> a -> a
    applyToTransientGameState event a =
      let GenericGameState {..} = get a
       in update a $ \(gs@TransientGameState {..}) -> case event of
            RenderEvent _ ->
              let gridsize :: Double
                  gridsize = 12
                  gridify :: Double -> Double
                  gridify = (* gridsize) . int2Double . floor . (/ gridsize)
                  placement = case gsCursorPos of Pos x' y' -> Pos (gridify x') (gridify y')
               in gs {gsLastPlacement = placement}
            _ -> gs
    applyToPersistentGameState :: (Has a GenericGameState, Has a TransientGameState, Has a PersistentGameState) => Event -> a -> a
    applyToPersistentGameState event a =
      let TransientGameState {..} = get a
          GenericGameState {..} = get a
       in update a $ \(gs@PersistentGameState {..}) -> case event of
            CursorPosEvent (Pos x y) ->
              let gridsize :: Double
                  gridsize = 12
                  gridify :: Double -> Double
                  gridify = (* gridsize) . int2Double . floor . (/ gridsize)
                  placement = Pos (gridify x) (gridify y)
               in gs
                    { gsBoard =
                        case gsUIMode of
                          TexturePlacementMode texture ->
                            case (`setMember` gsMousePressed) of
                              f | f MouseButton'1 -> Board $ mapInsert placement texture (unBoard gsBoard)
                              f | f MouseButton'2 -> Board $ mapDelete placement (unBoard gsBoard)
                              _ -> gsBoard
                          TextureMoveMode -> gsBoard
                    }
            KeyEvent key KeyState'Pressed ->
              gs
                { gsUIMode = case key of
                    Key'1 -> TexturePlacementMode FloorPlate
                    Key'2 -> TexturePlacementMode TopWall
                    Key'3 -> TextureMoveMode
                    _ -> gsUIMode
                }
            RenderEvent time ->
              if OneTimeEffect Reset `setMember` gsActions
                then gs {gsBoard = mempty}
                else
                  let picosecs = timeDiffPico gsLastLoopTime time
                      timePassed = int2Double (fromIntegral picosecs) / 1000 / 1000 / 1000 / 1000
                      distancePerSec = 100
                      d = timePassed * distancePerSec
                      Pos x y = gsMainCharacterPosition
                      newY = if MovementAction Up `setMember` gsActions then y - d else if MovementAction Down `setMember` gsActions then y + d else y
                      newX = if MovementAction Left' `setMember` gsActions then x - d else if MovementAction Right' `setMember` gsActions then x + d else x
                   in gs {gsMainCharacterPosition = Pos newX newY}
            _ -> gs
