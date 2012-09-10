module Tecs.Editor.Selection where

import Tecs.Types
import Tecs.Buffer
import Tecs.Editor.Cursor
import Tecs.Editor.Undo (pushUndo)
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
  in case openRange selSt of
    Just rangeStartPos ->
      let rangeStopPos = insertPos st
          rangePoss    = sort [rangeStartPos, rangeStopPos]
          newRange     = (rangePoss !! 0, rangePoss !! 1)
      in if (fst newRange) /= (snd newRange)
         then st { selState = selSt { ranges = newRange:(ranges selSt),
                                      openRange = Nothing } }
         else st
    Nothing -> st

startOrFinishOrCancelSelecting :: SimpleEditor -> SimpleEditor
startOrFinishOrCancelSelecting st =
  let selSt = selState st
      oRange = openRange selSt
  in case oRange of
    Nothing -> startSelecting st
    Just p | p == (insertPos st) -> cancelSelecting st
           | otherwise -> finishSelecting st

applyIfReasonableSelection :: (SimpleEditor -> SimpleEditor) -> SimpleEditor -> SimpleEditor
applyIfReasonableSelection f st =
  let st' = finishSelecting st
      rs  = (ranges . selState) st'
      firstRange = rs !! 0
  in if not $ null rs
     then f st'
     else st'

deleteSelection :: SimpleEditor -> SimpleEditor
deleteSelection =
  let delSel st =
        let selSt      = selState st
            rs         = ranges selSt
            firstRange = rs !! 0
            buf        = buffer st
        in (pushUndo st) { buffer    = delSelection buf firstRange,
                           selState  = selSt { ranges = drop 1 rs },
                           cursorPos = posWithinBuffer buf (fst firstRange) }
  in applyIfReasonableSelection delSel

forgetOpenRange :: SimpleEditor -> SimpleEditor
forgetOpenRange st =
  st { selState = (selState st) { openRange = Nothing } }

forgetRanges :: SimpleEditor -> SimpleEditor
forgetRanges st =
  st { selState = (selState st) { ranges = [] } }

forgetOpenRangeOrRanges :: SimpleEditor -> SimpleEditor
forgetOpenRangeOrRanges st =
  let selSt = selState st
  in case openRange selSt of
    Just _    -> forgetOpenRange st
    otherwise -> forgetRanges st

copyReasonableSelection :: GlobalState -> GlobalState
copyReasonableSelection gst =
  let ed    = finishSelecting (editor gst)
      selSt = selState ed
      sel   = (ranges selSt) !! 0
      buf   = buffer ed
  in if null (ranges selSt)
     then gst
     else gst { clipboard = (getSelection buf sel):(clipboard gst),
                editor = ed }

pasteAtInsertPos :: GlobalState -> GlobalState
pasteAtInsertPos gst
  | null (clipboard gst) = gst
  | otherwise =
      let ed = editor gst
          buf = buffer ed
          iPos = insertPos ed
          pasteSeq = (clipboard gst) !! 0
      in gst { editor = ed { buffer = insertLineSeqIntoBuffer buf iPos pasteSeq } }

