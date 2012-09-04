{-# LANGUAGE NoImplicitPrelude #-}

module Tecs.Editor where

import Prelude as P
import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO


import Tecs.Types as TT
import Tecs.Text
import Tecs.Buffer
import Tecs.Display

import Control.Monad.State

data SimpleEditor = SimpleEditor {
  undoBuffers :: [(Buffer, Pos)],
  lastSavePtr :: Int,
  buffer :: Buffer,
  cursorPos :: Pos,
  fileName :: String,
  lineScroll :: Int,
  viewHeight :: Int
  }
defaultSimpleEditor :: SimpleEditor
defaultSimpleEditor =
  SimpleEditor [] 0 (strToBuffer "") (Pos 0 0) "" 0 24

instance Editor SimpleEditor where
  render editor height width = do
    let displayedBuffer = bufferDropLines (lineScroll editor) (buffer editor)
    renderBuffer Crop displayedBuffer height width
    setCursor (screenPos editor)
  respond editor evt = (lookupWithDefault evtMap evt) evt editor

type SimpleEditorAction = State SimpleEditor ()

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

insertPos :: SimpleEditor -> Pos
insertPos se = let cPos = cursorPos se
                   l = line cPos
                   r = min (row cPos) (length $ lineAt l $ buffer se)
               in Pos l r

screenPos :: SimpleEditor -> Pos
screenPos se = let iPos = insertPos se
                   screenLine = (line iPos) - (lineScroll se)
                   isOnScreen = screenLine >= 0
               in if isOnScreen
                  then iPos { line = screenLine }
                  else Pos 0 0

advance pos = pos { row = (row pos) + 1}
retreat pos = pos { row = max 0 (row pos - 1)}

insertChar :: Char -> SimpleEditor -> SimpleEditor
insertChar c st =
  let cursor = insertPos st
      buf = buffer st
  in (pushUndo st) { buffer = insertCharIntoBuffer buf cursor c,
                     cursorPos = advance cursor }

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
              cursRow = length (lineAt (l - 1) concattedBuf)
          in st' { buffer = concattedBuf,
                   cursorPos = Pos (l - 1) cursRow }
     else st' { buffer = deleteCharFromBuffer buf cursor,
                cursorPos = retreat cursor }

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

fixScroll ed =
  let cp = cursorPos ed
      l  = line cp
      ls = lineScroll ed
      h  = viewHeight ed
      isBefore = l  < ls
      isAfter  = l >= ls + h
      newLineScroll
        | isBefore  = l
        | isAfter   = l - h + 1
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
      lenOfLineBefore = length $ lineAt (l - 1) $ buffer ed
  in if r > 0
     then ed { cursorPos = cp { row = r - 1 }}
     else if l > 0
          then fixScroll $ ed { cursorPos = Pos (l - 1) lenOfLineBefore }
          else ed

cursorRight ed =
  let cp = insertPos ed
      l = line cp
      r = row cp
      lenCurLine = length $ lineAt l $ buffer ed
  in if r < lenCurLine
     then ed { cursorPos = cp { row = r + 1 } }
     else if l < (numLines $ buffer ed)
          then fixScroll $ ed { cursorPos = Pos (l + 1) 0 }
          else ed

cursorEndOfLine ed =
  let cp = cursorPos ed
      nextRowPos = (length $ lineAt (line cp) (buffer ed))
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

ignoreEvt :: (SimpleEditor -> SimpleEditor) -> Event -> SimpleEditor -> SimpleEditor
ignoreEvt f evt ed = f ed

ie = ignoreEvt

undo :: SimpleEditor -> SimpleEditor
undo = fixScroll . popUndo

handleOther evt st = case evt of
  KeyEvent (KeyChar c) -> insertChar c st
  otherwise -> st

evtMap :: DefaultMap Event (Event -> SimpleEditor -> SimpleEditor)
evtMap = defaultMapFromList [
  (KeyEvent KeyUp,             ie cursorUp),
  (KeyEvent KeyDown,           ie cursorDown),
  (KeyEvent KeyLeft,           ie cursorLeft),
  (KeyEvent KeyRight,          ie cursorRight),
  (KeyEvent KeyEnter,          ie insertLinebreak),
  (KeyEvent KeyDel,            ie deleteChar),
  (KeyEvent KeyPageDown,       ie cursorPageDown),
  (KeyEvent KeyPageUp,         ie cursorPageUp),
  (KeyEvent $ KeyCtrlChar 'A', ie cursorBeginningOfLine),
  (KeyEvent $ KeyCtrlChar 'E', ie cursorEndOfLine),
  (KeyEvent $ KeyCtrlChar 'I', ie insertTab),
  (KeyEvent $ KeyCtrlChar 'Z', ie undo),
  (KeyEvent $ KeyCtrlChar 'K', ie killLine)
  ] handleOther


simpleEditorFromFile :: String -> IO (SimpleEditor)
simpleEditorFromFile filename = do
  s <- DTIO.readFile filename
  let buf = strToBuffer (DT.unpack s)
  return $ defaultSimpleEditor {
    buffer = buf,
    fileName = filename
    }

renderEditor :: Editor a => Box -> a -> IO ()
renderEditor b@(Box _ _ height width) editor =
  let (_, commands) = execRender (render editor height width)
  in do mapM (drawToScreen b) commands
        return ()

data InfoLineEditor = InfoLineEditor {
  infoBuffer :: Buffer
  }
instance Editor InfoLineEditor where
  render editor height width =
    renderBuffer Crop (infoBuffer editor) height width
  respond editor evt = editor

setInfoLineContent infoLineEditor str =
  infoLineEditor { infoBuffer = strToBuffer str }

defaultInfoLineEditor = InfoLineEditor $ strToBuffer ""
