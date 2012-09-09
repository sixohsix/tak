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
import Tecs.Editor.Undo as Undo
import Tecs.Editor.Edit

import Control.Monad.State

isModified = Undo.isModified

instance Editor SimpleEditor where
  render editor height width = do
    let displayedBuffer = bufferDropLines (lineScroll editor) (buffer editor)
    renderBuffer Crop displayedBuffer height width
    setCursor (screenPos editor)
  respond editor evt = (lookupWithDefault evtMap evt) evt editor

ignoreEvt :: (SimpleEditor -> SimpleEditor) -> Event -> SimpleEditor -> SimpleEditor
ignoreEvt f evt ed = f ed

ie = ignoreEvt

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

