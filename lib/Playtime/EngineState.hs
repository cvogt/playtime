module Playtime.EngineState where

import GHC.Float (int2Double)
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime.Types
import Playtime.Util

makeInitialEngineState :: Scale -> Dimensions -> SystemTime -> EngineState
makeInitialEngineState scale dim time =
  EngineState
    { esCursorPos = Pos 0 0,
      esFps = 0,
      esKeysPressed = mempty,
      esMousePressed = mempty,
      esLastLoopTime = time,
      esLogicalDimensions = dim,
      esActions = mempty,
      esTimes = [],
      esTimePassed = 0,
      esWindowSize = scale |*| dim
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
    { esActions =
        -- clear triggers for one time side effects
        esActions es `difference` (setFromList $ fmap OneTimeEffect $ catMaybes $ fmap oneTimeEffectMay $ toList $ esActions es)
    }

stepEngineState :: EngineState -> Event -> EngineState
stepEngineState (clearOneTimeEffects -> gs@EngineState {..}) = \case
  WindowSizeEvent width height -> gs {esWindowSize = Dimensions (int2Double width) (int2Double height)}
  CursorPosEvent x y ->
    gs
      { esCursorPos =
          -- this ratio calculation leads to proper relative scaling on window resize
          -- FIXME: we still get distortion if aspect ration of resized window is different
          --        we should be able to fix that by adding black borders as needed
          let Scale {sx, sy} = esLogicalDimensions |/| esWindowSize
           in Pos (sx * x) (sy * y)
      }
  KeyEvent key KeyState'Pressed ->
    let pressed = setInsert key esKeysPressed
        matchingBindings = fromMaybe [] $ mapLookup key $ groupKeyBindings keyBindings
        actions = setFromList $ take 1 $ snd <$> filter (null . (`difference` pressed) . fst) matchingBindings
     in gs {esKeysPressed = pressed, esActions = esActions `union` actions}
  KeyEvent key KeyState'Released ->
    let pressed = setInsert key esKeysPressed
        matchingBindings = fromMaybe [] $ mapLookup key $ groupKeyBindings keyBindings
        actions = setFromList $ take 1 $ snd <$> filter (null . (`difference` pressed) . fst) matchingBindings
     in gs {esKeysPressed = setDelete key esKeysPressed, esActions = esActions `difference` actions}
  MouseEvent mb MouseButtonState'Pressed -> gs {esMousePressed = setInsert mb esMousePressed}
  MouseEvent mb MouseButtonState'Released -> gs {esMousePressed = setDelete mb esMousePressed}
  WindowCloseEvent -> gs {esActions = setFromList [Exit, OneTimeEffect Save] `union` esActions}
  RenderEvent time ->
    let picosecs = timeDiffPico esLastLoopTime time
        halfsec = 500 * 1000 * 1000 * 1000
     in gs
          { esLastLoopTime = time,
            esTimePassed = pico2Double picosecs,
            esTimes = if sum esTimes > halfsec then [] else picosecs : esTimes,
            esFps = if sum esTimes > halfsec then avg esTimes else esFps
          }
  _ -> gs
