{-# LANGUAGE NoImplicitPrelude #-}

module Tecs.Editor where

import Prelude as P
import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import System.Directory (doesFileExist)
import Control.Arrow ( (>>>) )

import Tecs.Types as TT
import Tecs.Text
import Tecs.Buffer
import Tecs.Display
import Tecs.Editor.Cursor
import Tecs.Editor.Undo as Undo
import Tecs.Editor.Edit
import Tecs.Editor.Selection

isModified = Undo.isModified

instance Editor SimpleEditor where
  render editor height width = do
    let displayedBuffer = bufferDropLines (lineScroll editor) (buffer editor)
    renderBuffer Crop displayedBuffer height width
    setCursor (screenPos editor)

ignoreEvt :: (SimpleEditor -> SimpleEditor) -> GlobalState -> IO GlobalState
ignoreEvt f gst = return $ gst { editor = f (editor gst) }

ie = ignoreEvt

handleOther :: Event -> GlobalState -> IO GlobalState
handleOther evt gst = case evt of
  KeyEvent (KeyChar c) -> (ignoreEvt $ insertChar c) gst
  otherwise -> return gst

editorEvtMap :: DefaultMap Event (GlobalState -> IO GlobalState)
editorEvtMap = defaultMapFromList [
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
  (KeyEvent $ KeyCtrlChar 'K', ie killLine),
  (KeyEvent $ KeyCtrlChar '@', ie startOrFinishOrCancelSelecting),
  (KeyEvent $ KeyCtrlChar 'X', \gst -> ((copyReasonableSelection >>> (ie deleteSelection)) gst) >>= tmpWriteClipboard),
  (KeyEvent $ KeyCtrlChar 'C', return . copyReasonableSelection),
  (KeyEvent $ KeyCtrlChar 'G', ie forgetOpenRangeOrRanges),
  (KeyEvent $ KeyCtrlChar 'V', return . pasteAtInsertPos),
  (KeyEvent KeyCtrlUp,         ie cursorPrevPara),
  (KeyEvent KeyCtrlDown,       ie cursorNextPara),
  (KeyEvent KeyCtrlRight,      ie cursorNextWord),
  (KeyEvent KeyCtrlLeft,       ie cursorPrevWord)
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

instance Editor InfoLineEditor where
  render editor height width = do
    invertText
    renderBuffer Crop (infoBuffer editor) height width

setInfoLineContent infoLineEditor str =
  infoLineEditor { infoBuffer = strToBuffer str }

