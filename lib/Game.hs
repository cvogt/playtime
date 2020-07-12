module Game where

import GHC.Float (int2Double)
import GHC.Real ((/), floor, fromIntegral)
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import SpaceMiner.Textures
import SpaceMiner.Types
import SpaceMiner.Util

makeInitialGameState :: Scale -> Dimensions -> SystemTime -> GameState
makeInitialGameState scale dim@Dimensions {width, height} time =
  ( GenericGameState
      { gsCursorPos = Pos 0 0,
        gsFps = 0,
        gsKeysPressed = mempty,
        gsMousePressed = mempty,
        gsLastLoopTime = time,
        gsLogicalDimensions = dim,
        gsActions = mempty,
        gsTimes = [],
        gsWindowSize = scale |*| dim
      },
    PersistentGameState
      { gsUIMode = TexturePlacementMode FloorPlate,
        gsCollisions = (Nothing, Nothing, Nothing, Nothing),
        gsFloor = mempty,
        gsRoom = mempty,
        gsLastPlacement = Pos 0 0,
        gsMainCharacterPosition = Pos (width / 2) (height / 2)
      }
  )

keyBindings :: Map Key [(Set Key, Action)]
keyBindings = mapFromList $ groups <&> \l@(h :| _) -> (fst h, first setFromList <$> (join . toList $ snd <$> l))
  where
    groups :: [NonEmpty (Key, [([Key], Action)])]
    groups = groupAllWith fst $ join $ keyBindingsRaw <&> (\b@(keys', _) -> (,[b]) <$> keys')
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
  foldl (.) id (flap [applyToGenericGameState, applyToPersistentGameState] event')
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
        WindowSizeEvent width height -> gs {gsWindowSize = Dimensions (int2Double width) (int2Double height)}
        CursorPosEvent x y ->
          gs
            { gsCursorPos =
                -- this ratio calculation leads to proper relative scaling on window resize
                -- FIXME: we still get distortion if aspect ration of resized window is different
                --        we should be able to fix that by adding black borders as needed
                let Scale {sx, sy} = gsLogicalDimensions |/| gsWindowSize
                 in Pos (sx * x) (sy * y)
            }
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
    gridsize :: Num n => n
    gridsize = 12
    applyToPersistentGameState :: (Has a GenericGameState, Has a PersistentGameState) => Event -> a -> a
    applyToPersistentGameState event a =
      let GenericGameState {..} = get a
       in update a $ \(gs@PersistentGameState {..}) -> case event of
            CursorPosEvent _ _ ->
              let Pos x y = gsCursorPos
                  gridify :: Double -> Double
                  gridify = (* gridsize) . int2Double . floor . (/ gridsize)
                  placement = Pos (gridify x) (gridify y)
               in gs
                    { gsLastPlacement = placement,
                      gsFloor =
                        case gsUIMode of
                          TexturePlacementMode texture ->
                            case (`setMember` gsMousePressed) of
                              f | f MouseButton'1 && texture == FloorPlate -> Board $ mapInsert placement texture (unBoard gsFloor)
                              f | f MouseButton'2 -> Board $ mapDelete placement (unBoard gsFloor)
                              _ -> gsFloor
                          TextureMoveMode -> gsFloor,
                      gsRoom =
                        case gsUIMode of
                          TexturePlacementMode texture ->
                            case (`setMember` gsMousePressed) of
                              f | f MouseButton'1 && texture /= FloorPlate -> Board $ mapInsert placement texture (unBoard gsRoom)
                              f | f MouseButton'2 -> Board $ mapDelete placement (unBoard gsRoom)
                              _ -> gsRoom
                          TextureMoveMode -> gsRoom
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
                then gs {gsFloor = mempty, gsRoom = mempty}
                else
                  let picosecs = timeDiffPico gsLastLoopTime time
                      timePassed = int2Double (fromIntegral picosecs) / 1000 / 1000 / 1000 / 1000
                      distancePerSec = 100
                      charSize = gridsize
                      d = timePassed * distancePerSec
                      Pos x y = gsMainCharacterPosition
                      newY = if MovementAction Up `setMember` gsActions then y - d else if MovementAction Down `setMember` gsActions then y + d else y
                      newX = if MovementAction Left' `setMember` gsActions then x - d else if MovementAction Right' `setMember` gsActions then x + d else x
                      newPos = Pos newX newY
                      tileArea pos = Area pos charSize
                      newArea = tileArea newPos
                      collisions = filter (newArea `collidesWith`) $ tileArea <$> (keys $ unBoard gsRoom)
                      (nw, sw, se, ne) = corners newArea
                      nwCollision = find (nw `isWithin`) collisions
                      swCollision = find (sw `isWithin`) collisions
                      seCollision = find (se `isWithin`) collisions
                      neCollision = find (ne `isWithin`) collisions
                      fixedPos = case (nwCollision, swCollision, seCollision, neCollision) of
                        (Just _, Just (Area (Pos cX _) (Dimensions cW _)), Nothing, Nothing) -> Pos (cX + cW) newY
                        (Nothing, Nothing, Just (Area (Pos cX _) _), Just _) -> Pos (cX - width charSize) newY
                        (Nothing, Just _, Just (Area (Pos _ cY) _), Nothing) -> Pos newX (cY - height charSize)
                        (Just _, Nothing, Nothing, Just (Area (Pos _ cY) (Dimensions _ cH))) -> Pos newX (cY + cH)
                        (Nothing, Nothing, Nothing, Nothing) -> newPos
                        _ -> gsMainCharacterPosition
                   in gs {gsCollisions = (nwCollision, swCollision, seCollision, neCollision), gsMainCharacterPosition = fixedPos}
            _ -> gs
