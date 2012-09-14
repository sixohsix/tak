module Tecs.Editor.Edit where

import Tecs.Types
import Tecs.Buffer
import Tecs.Editor.Cursor
import Tecs.Editor.Undo


insertChar :: Char -> SimpleEditor -> SimpleEditor
insertChar c st =
  let cursor = insertPos st
      buf = buffer st
  in (pushUndo st) { buffer = insertCharIntoBuffer buf cursor c,
                     cursorPos = cursor { row = (row cursor) + 1 } }

insertTab :: SimpleEditor -> SimpleEditor
insertTab st =
  let insertSpace = insertChar ' '
  in insertSpace $ insertSpace st

deleteChar :: SimpleEditor -> SimpleEditor
deleteChar st =
  let cursor = insertPos st
      l = line cursor
      r = row cursor
      buf = buffer st
      st' = pushUndo st
  in if r == 0 && l > 0
     then let concattedBuf = concatLine buf (line cursor)
              cursRow = length (lineAt (l - 1) buf)
          in st' { buffer = concattedBuf,
                   cursorPos = Pos (l - 1) cursRow }
     else st' { buffer = deleteCharFromBuffer buf cursor,
                cursorPos = cursor { row = (row cursor) - 1 } }

insertLinebreak :: SimpleEditor -> SimpleEditor
insertLinebreak st =
  let cursor = insertPos st
      buf = buffer st
  in fixScroll $ (pushUndo st) {
    buffer = insertLinebreakIntoBuffer buf cursor,
    cursorPos = Pos { line = (line cursor) + 1,
                      row = 0 } }

killLine :: SimpleEditor -> SimpleEditor
killLine st =
  let buf = buffer st
      l = (line $ insertPos st)
  in (pushUndo st) {
    buffer = deleteLine buf l
    }

