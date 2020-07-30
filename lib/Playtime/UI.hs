module Playtime.UI where

import Data.Aeson (FromJSON, ToJSON)
import "GLFW-b" Graphics.UI.GLFW
import My.Prelude
import Playtime.Types

data DragAndDrop = DragAndDrop Pos Dimensions deriving (Show, Generic, NFData, ToJSON, FromJSON)

dragAndDrop ::
  EngineState ->
  gs ->
  MouseButton ->
  [Area] ->
  ([Pos] -> gs -> gs) ->
  Maybe DragAndDrop ->
  (Maybe DragAndDrop -> gs -> gs) ->
  Event ->
  gs
dragAndDrop EngineState {..} gs mb areas setPoss dragAndDrop' setDragAndDrop =
  let toPos = \(pos, _) -> pos
      poss = toPos <$> areas
   in \case
        MouseEvent mb' MouseButtonState'Pressed
          | mb == mb' ->
            let clicked = find (isWithin esCursorPos) areas
             in gs
                  & (setDragAndDrop $ clicked <&> \(pos, _) -> DragAndDrop pos $ pos |-| esCursorPos)
                  & (setPoss $ poss \\ catMaybes [toPos <$> clicked])
        MouseEvent mb' MouseButtonState'Released
          | mb == mb' ->
            gs
              & (setDragAndDrop Nothing)
              & (setPoss $ catMaybes [dragAndDrop' <&> \(DragAndDrop pos _) -> pos] <> poss)
        CursorPosEvent cursor ->
          gs
            & (setDragAndDrop $ dragAndDrop' <&> (\(DragAndDrop _ offset) -> DragAndDrop (cursor |+| offset) offset))
        _ -> gs

showDragAndDrop :: Maybe DragAndDrop -> (Pos -> a) -> [a]
showDragAndDrop dragAndDrop' sprite' = catMaybes [dragAndDrop' <&> (\(DragAndDrop pos _) -> sprite' pos)]

deleteOnClick :: EngineState -> gs -> MouseButton -> [Area] -> ([Pos] -> gs -> gs) -> Event -> gs
deleteOnClick EngineState {..} gs mb areas setPoss =
  let toPos = \(pos, _) -> pos
      poss = toPos <$> areas
   in \case
        MouseEvent mb' MouseButtonState'Pressed
          | mb == mb' ->
            let clicked = find (isWithin esCursorPos) areas
             in gs & (setPoss $ poss \\ catMaybes [toPos <$> clicked])
        _ -> gs
