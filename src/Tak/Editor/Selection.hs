module Tak.Editor.Selection where

import Tak.Types
import Tak.GlobalState
import Tak.Buffer
import qualified Tak.Buffer.Line as L
import Tak.Range
import Tak.Editor.Cursor
import Tak.Editor.Undo (pushUndo)
import Data.List (sort)
import qualified Data.Sequence as Seq
import Control.Arrow ( (>>>) )
import Control.Lens


startSelecting :: SimpleEditor -> SimpleEditor
startSelecting st =
  st { selState = (selState st) { openRange = Just (insertPos st) } }

cancelSelecting :: SimpleEditor -> SimpleEditor
cancelSelecting st =
  st { selState = (selState st) { openRange = Nothing } }


currentSelection :: SimpleEditor -> Maybe Range
currentSelection st =
  let selSt              = selState st
      Just rangeStartPos = openRange selSt
  in case openRange selSt of
    Just rangeStartPos ->
      let rangeStopPos = insertPos st
      in if rangeStartPos /= rangeStopPos
         then Just $ makeRange rangeStartPos rangeStopPos
         else Nothing
    Nothing -> Nothing


applyIfSelection :: (Range -> SimpleEditor -> SimpleEditor) -> SimpleEditor -> SimpleEditor
applyIfSelection f ed = maybe ed (\r -> f r ed) (currentSelection ed)


deleteSelection :: SimpleEditor -> SimpleEditor
deleteSelection = cancelSelecting . applyIfSelection deleteRange


deleteRange :: Range -> SimpleEditor -> SimpleEditor
deleteRange r ed =
  let buf = buffer ed
  in (pushUndo ed) { buffer    = delSelection buf (asTuple r),
                     cursorPos = posWithinBuffer buf (startPos r) }

copySelection :: GlobalState -> GlobalState
copySelection gst = 
  let ed = activeEditor gst
  in maybe gst (\r -> set clipboard ( (getSelection (buffer ed) (asTuple r)):(view clipboard gst) ) gst) (currentSelection ed)


pasteAtInsertPos :: GlobalState -> GlobalState
pasteAtInsertPos gst
  | Nothing == (pasteable gst) = gst
  | otherwise =
      let ed = activeEditor gst
          buf = buffer ed
          iPos = insertPos ed
          Pos l r = iPos
          Just pasteSeq = pasteable gst
          lPasteSeq = Seq.length pasteSeq
          isOneLinePaste = lPasteSeq == 1
          lastLineLen = L.length $ Seq.index pasteSeq (lPasteSeq - 1)
      in (set editor $ (pushUndo ed) { buffer = insertLineSeqIntoBuffer buf iPos pasteSeq,
                                       cursorPos = Pos (l + (Seq.length pasteSeq) - 1)
                                                       (if isOneLinePaste then (r + lastLineLen) else lastLineLen) }) gst

tmpWriteClipboard gst = do
  writeFile "./clip.tmp" $ lineSeqToStr ((view clipboard gst) !! 0)
  return gst

