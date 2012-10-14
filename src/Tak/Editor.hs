{-# LANGUAGE NoImplicitPrelude #-}

module Tak.Editor where

import Prelude as P
import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import System.Directory (doesFileExist)
import System.IO
import Control.Arrow ( (>>>) )
import Control.Monad ( (>=>) )
import Control.Lens
import Data.Map as Map

import Tak.Types as TT
import Tak.Range
import Tak.Text
import Tak.Buffer
import Tak.Display
import Tak.Editor.Cursor
import Tak.Editor.Undo
import Tak.Editor.Edit
import Tak.Editor.Selection
import Tak.Editor.Replace
import Tak.Config

instance Editor SimpleEditor where
  render editor height width = do
    let lScroll = lineScroll editor
        displayedBuffer = bufferDropLines (lineScroll editor) (buffer editor)
        mRange = maybe Nothing (\r -> Just $ r `shiftRange` (Pos (-lScroll) 0)) (currentSelection editor)
    renderBuffer Crop displayedBuffer mRange height width
    setCursor (screenPos editor)

ignoreEvt :: (SimpleEditor -> SimpleEditor) -> GlobalState -> IO GlobalState
ignoreEvt f evt = return $ over editor f evt

ie = ignoreEvt

insertIfChar :: Event -> GlobalState -> IO GlobalState
insertIfChar evt gst = case evt of
  KeyEvent (KeyChar c) -> (ignoreEvt $ insertChar c) gst
  otherwise -> return gst

readOnlyCommands = Map.fromList [
  (KeyEvent KeyUp,             ie cursorUp),
  (KeyEvent KeyDown,           ie cursorDown),
  (KeyEvent KeyLeft,           ie cursorLeft),
  (KeyEvent KeyRight,          ie cursorRight),
  (KeyEvent KeyPageDown,       ie cursorPageDown),
  (KeyEvent KeyPageUp,         ie cursorPageUp),
  (KeyEvent $ KeyCtrlChar 'A', ie cursorBeginningOfLine),
  (KeyEvent KeyHome,           ie cursorBeginningOfLine),
  (KeyEvent $ KeyCtrlChar 'E', ie cursorEndOfLine),
  (KeyEvent KeyEnd,            ie cursorEndOfLine),
  (KeyEvent $ KeyCtrlChar '@', ie startSelecting),
  (KeyEvent $ KeyCtrlChar 'C', (return . copySelection) >=> (ie cancelSelecting)),
  (KeyEvent $ KeyCtrlChar 'G', ie cancelSelecting),
  (KeyEvent KeyCtrlUp,         ie cursorPrevPara),
  (KeyEvent KeyCtrlDown,       ie cursorNextPara),
  (KeyEvent KeyCtrlRight,      ie cursorNextWord),
  (KeyEvent KeyCtrlLeft,       ie cursorPrevWord),
  (KeyEvent $ KeyEscaped $ KeyUp, ie cursorPrevPara),
  (KeyEvent $ KeyEscaped $ KeyDown, ie cursorNextPara),
  (KeyEvent $ KeyEscaped $ KeyRight, ie cursorNextWord),
  (KeyEvent $ KeyEscaped $ KeyLeft, ie cursorPrevWord),
  (KeyEvent KeyCtrlHome,       ie cursorFirstPos),
  (KeyEvent KeyCtrlEnd,        ie cursorLastPos)
  ]

editCommands = Map.fromList [
  (KeyEvent KeyEnter,          ie insertLinebreak),
  (KeyEvent KeyDel,            ie deleteChar),
  (KeyEvent $ KeyCtrlChar 'I', ie insertTab),
  (KeyEvent $ KeyCtrlChar 'Z', ie undo),
  (KeyEvent $ KeyCtrlChar 'K', ie killLine),
  (KeyEvent $ KeyCtrlChar 'X', \gst -> ((copySelection >>> (ie deleteSelection)) gst)),
  (KeyEvent $ KeyCtrlChar 'V', return . pasteAtInsertPos),
  (KeyEvent $ KeyCtrlChar 'P', replaceRegionWithShellCmd "echo hello")
  ]  

defaultModeHandler :: Event -> GlobalState -> IO GlobalState
defaultModeHandler evt = findWithDefault (insertIfChar evt) evt (editCommands `Map.union` readOnlyCommands)

defaultEditorMode = Mode defaultModeHandler


simpleEditorFromFile :: String -> IO (SimpleEditor)
simpleEditorFromFile filename = do
  fileExists <- doesFileExist filename
  s <- if fileExists
       then do
         h <- openFile filename ReadMode
         hSetEncoding h utf8_bom
         hSetNewlineMode h universalNewlineMode
         contents <- DTIO.hGetContents h
         hClose h
         return contents
       else return DT.empty
  let buf = textToBuffer s
  pos <- getInitialPosition filename
  return $ fixScroll $ defaultSimpleEditor {
    buffer = buf,
    fileName = filename,
    cursorPos = pos
    }

