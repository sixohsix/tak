module Tak.Editor.Cursor where

import qualified Tak.Buffer.Line as L

import Tak.Util
import Tak.Types
import Tak.Buffer

insertPos :: SimpleEditor -> Pos
insertPos ed = posWithinBuffer (buffer ed) (cursorPos ed)

screenPos :: SimpleEditor -> Pos
screenPos se = let iPos = insertPos se
                   Pos y x = iPos
                   screenLine = (line iPos) - (lineScroll se)
                   isOnScreen = screenLine >= 0
                   width = L.lineWidth (L.take x (lineAt y $ buffer se))
               in if isOnScreen
                  then iPos { line = screenLine, row = width }
                  else Pos 0 0


fixScroll = fixScrollN 0
fixScrollCentered ed = fixScrollN (viewHeight ed `div` 2) ed

fixScrollN offset ed =
  let cp = cursorPos ed
      l  = line cp
      ls = lineScroll ed
      h  = viewHeight ed
      isBefore = l  < ls
      isAfter  = l >= ls + h
      newLineScroll
        | isBefore  = l
        | isAfter   = l - h + 1 + offset
        | otherwise = ls
  in ed { lineScroll = newLineScroll }

cursorDown ed =
  let cp = cursorPos ed
      nextLinePos = min (lastLineIdx $ buffer ed) (line cp + 1)
  in fixScroll $ ed { cursorPos = cp { line = nextLinePos }}

cursorUp ed =
  let cp = cursorPos ed
      nextLinePos = max 0 (line cp - 1)
  in fixScroll $ ed { cursorPos = cp { line = nextLinePos }}

cursorLeft ed =
  let cp = insertPos ed
      l = line cp
      r = row cp
      lenOfLineBefore = L.length $ lineAt (l - 1) $ buffer ed
  in if r > 0
     then ed { cursorPos = cp { row = r - 1 }}
     else if l > 0
          then fixScroll $ ed { cursorPos = Pos (l - 1) lenOfLineBefore }
          else ed

cursorRight ed =
  let cp = insertPos ed
      l = line cp
      r = row cp
      lenCurLine = L.length $ lineAt l $ buffer ed
  in if r < lenCurLine
     then ed { cursorPos = cp { row = r + 1 } }
     else if l < (lastLineIdx $ buffer ed)
          then fixScroll $ ed { cursorPos = Pos (l + 1) 0 }
          else ed

cursorEndOfLine ed =
  let cp = cursorPos ed
      nextRowPos = (L.length $ lineAt (line cp) (buffer ed))
  in ed { cursorPos = cp { row = nextRowPos }}

cursorBeginningOfLine ed = ed { cursorPos = (cursorPos ed) { row = 0 } }

cursorPageDown ed =
  let cp = cursorPos ed
      l = line cp
      pageLen = (viewHeight ed) - 3
      lastBufLineIdx = lastLineIdx $ buffer ed
  in fixScroll $ ed { cursorPos = cp { line = min (l + pageLen) lastBufLineIdx } }

cursorPageUp ed =
  let cp = cursorPos ed
      l = line cp
      pageLen = (viewHeight ed) - 3
  in fixScroll $ ed { cursorPos = cp { line = max 0 (l - pageLen) } }

updateCursor :: (Buffer -> Pos -> Pos) -> SimpleEditor -> SimpleEditor
updateCursor f ed = fixScroll $ ed { cursorPos = f (buffer ed) (insertPos ed) }

cursorNextPara = updateCursor posNextPara
cursorPrevPara = updateCursor posPrevPara
cursorNextWord = updateCursor posNextWord
cursorPrevWord = updateCursor posPrevWord
cursorFirstPos = updateCursor posFirstPos
cursorLastPos  = updateCursor posLastPos

