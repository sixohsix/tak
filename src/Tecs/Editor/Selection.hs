module Tecs.Editor.Selection where

import Tecs.Types
import Tecs.Editor.Cursor
import Data.List (sort)


startSelecting :: SimpleEditor -> SimpleEditor
startSelecting st =
  st { selState = (selState st) { openRange = Just (insertPos st) } }

cancelSelecting :: SimpleEditor -> SimpleEditor
cancelSelecting st =
  st { selState = (selState st) { openRange = Nothing } }

finishSelecting :: SimpleEditor -> SimpleEditor
finishSelecting st =
  let selSt              = selState st
      Just rangeStartPos = openRange selSt
      rangeStopPos       = insertPos st
      rangePoss          = sort [rangeStartPos, rangeStopPos]
      newRange           = (rangePoss !! 0, rangePoss !! 1)
      newSelSt           = selSt { ranges = newRange:(ranges selSt) }
  in maybe st (\_ -> st { selState = newSelSt }) (openRange selSt)

startOrFinishOrCancelSelecting :: SimpleEditor -> SimpleEditor
startOrFinishOrCancelSelecting st =
  let selSt = selState st
      oRange = openRange selSt
  in case oRange of
    Nothing -> startSelecting st
    Just p | p == (insertPos st) -> cancelSelecting st
           | otherwise -> finishSelecting st
