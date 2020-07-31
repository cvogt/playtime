module Playtime.EngineState where

import GHC.Float
import My.IO
import My.Prelude
import Playtime.Types
import Playtime.Util

data EngineConfig = EngineConfig
  { ecDim :: Dim,
    ecScale :: Scale,
    ecVisualize :: EngineState -> IO [Sprite],
    ecStepGameState :: EngineState -> Event -> IO (),
    ecCheckIfContinue :: EngineState -> IO Bool,
    ecGameDebugInfo :: EngineState -> IO [[Char]]
  }

data EngineState = EngineState
  { esCursorPos :: Pos,
    esFps :: Double,
    esWindowDimensions :: Dim,
    esKeysPressed :: Set Key,
    esMousePressed :: Set MouseButton,
    esLastLoopTime :: SystemTime,
    esActions :: Set Action,
    esTimes :: [Integer],
    esTimePassed :: Scale,
    esWindowSize :: Dim
  }
  deriving (Show, Generic, NFData)

makeInitialEngineState :: Scale -> Dim -> SystemTime -> EngineState
makeInitialEngineState scale dim time =
  EngineState
    { esCursorPos = originPos,
      esFps = 0,
      esKeysPressed = mempty,
      esMousePressed = mempty,
      esLastLoopTime = time,
      esWindowDimensions = dim,
      esActions = mempty,
      esTimes = [],
      esTimePassed = 0,
      esWindowSize = scale *| dim
    }

gameExitRequested :: EngineState -> Bool
gameExitRequested es = Exit `elem` (esActions es)

clearOneTimeEffects :: EngineState -> EngineState
clearOneTimeEffects es =
  es
    { esActions =
        -- clear triggers for one time side effects
        esActions es `difference` (setFromList $ fmap OneTimeEffect $ catMaybes $ fmap oneTimeEffectMay $ toList $ esActions es)
    }

stepEngineState :: EngineState -> Event -> EngineState
stepEngineState (clearOneTimeEffects -> gs@EngineState {..}) = \case
  WindowSizeEvent width height -> gs {esWindowSize = (Relative $ int2Double width, Relative $ int2Double height)}
  CursorPosEvent pos ->
    gs
      { esCursorPos =
          -- this ratio calculation leads to proper relative scaling on window resize
          -- FIXME: we still get distortion if aspect ration of resized window is different
          --        we should be able to fix that by adding black borders as needed
          let scale = esWindowDimensions |/| esWindowSize :: Scale
              relPos = (pos |-| originPos) :: Dim
           in originPos |+ scale *| relPos
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
            esTimePassed = let s = pico2Double picosecs in (Factor s, Factor s),
            esTimes = if sum esTimes > halfsec then [] else picosecs : esTimes,
            esFps = if sum esTimes > halfsec then avg esTimes else esFps
          }
  _ -> gs
