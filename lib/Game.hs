module Game where

import GHC.Float (int2Double)
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime.Types
import Playtime.Util

makeInitialEngineState :: Scale -> Dimensions -> SystemTime -> EngineState
makeInitialEngineState scale dim time =
  EngineState
    { gsCursorPos = Pos 0 0,
      gsFps = 0,
      gsKeysPressed = mempty,
      gsMousePressed = mempty,
      gsLastLoopTime = time,
      gsLogicalDimensions = dim,
      gsActions = mempty,
      gsTimes = [],
      gsWindowSize = scale |*| dim
    }

groupKeyBindings :: [([Key], Action)] -> Map Key [(Set Key, Action)]
groupKeyBindings keyBindingsRaw = mapFromList $ groups <&> \l@(h :| _) -> (fst h, first setFromList <$> (join . toList $ snd <$> l))
  where
    groups :: [NonEmpty (Key, [([Key], Action)])]
    groups = groupAllWith fst $ join $ keyBindingsRaw <&> (\b@(keys', _) -> (,[b]) <$> keys')

keyBindings :: [([Key], Action)]
keyBindings =
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

clearOneTimeEffects :: EngineState -> EngineState
clearOneTimeEffects es =
  es
    { gsActions =
        -- clear triggers for one time side effects
        gsActions es `difference` (setFromList $ fmap OneTimeEffect $ catMaybes $ fmap oneTimeEffectMay $ toList $ gsActions es)
    }

stepEngineState :: EngineState -> Event -> EngineState
stepEngineState (clearOneTimeEffects -> gs@EngineState {..}) = \case
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
        matchingBindings = fromMaybe [] $ mapLookup key $ groupKeyBindings keyBindings
        actions = setFromList $ take 1 $ snd <$> filter (null . (`difference` pressed) . fst) matchingBindings
     in gs {gsKeysPressed = pressed, gsActions = gsActions `union` actions}
  KeyEvent key KeyState'Released ->
    let pressed = setInsert key gsKeysPressed
        matchingBindings = fromMaybe [] $ mapLookup key $ groupKeyBindings keyBindings
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
