module Tak.Editor.Undo where

import Tak.Types
import Tak.Editor.Cursor


pushUndo :: SimpleEditor -> SimpleEditor
pushUndo st =
  st { undoBuffers = (buffer st, cursorPos st):(undoBuffers st),
       lastSavePtr = (lastSavePtr st) + 1 }

popUndo :: SimpleEditor -> SimpleEditor
popUndo st =
  let (lastBuf, lastPos) = (undoBuffers st) !! 0
  in if not $ null (undoBuffers st)
     then st { buffer = lastBuf,
               cursorPos = lastPos,
               undoBuffers = drop 1 (undoBuffers st),
               lastSavePtr = (lastSavePtr st) - 1 }
     else st

isModified :: SimpleEditor -> Bool
isModified ed = (lastSavePtr ed) /= 0

undo :: SimpleEditor -> SimpleEditor
undo = fixScroll . popUndo
