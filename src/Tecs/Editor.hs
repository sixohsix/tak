{-# LANGUAGE NoImplicitPrelude #-}

module Tecs.Editor where

import Prelude as P
import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import System.Directory (doesFileExist)

import Tecs.Types as TT
import Tecs.Text
import Tecs.Buffer
import Tecs.Display
import Tecs.Editor.Cursor

import Control.Monad.State

instance Editor SimpleEditor where
  render editor height width = do
    let displayedBuffer = bufferDropLines (lineScroll editor) (buffer editor)
    renderBuffer Crop displayedBuffer height width
    setCursor (screenPos editor)
  respond editor evt = (lookupWithDefault evtMap evt) evt editor

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
              cursRow = length (lineAt (l - 1) concattedBuf)
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
  fileExists <- doesFileExist filename
  s <- if fileExists
       then DTIO.readFile filename
       else return DT.empty
  let buf = strToBuffer (DT.unpack s)
  return $ defaultSimpleEditor {
    buffer = buf,
    fileName = filename
    }

data InfoLineEditor = InfoLineEditor {
  infoBuffer :: Buffer
  }
instance Editor InfoLineEditor where
  render editor height width = do
    invertText
    renderBuffer Crop (infoBuffer editor) height width
  respond editor evt = editor

setInfoLineContent infoLineEditor str =
  infoLineEditor { infoBuffer = strToBuffer str }

defaultInfoLineEditor = InfoLineEditor defaultBuffer

