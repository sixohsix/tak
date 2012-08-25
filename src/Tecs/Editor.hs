{-# LANGUAGE NoImplicitPrelude #-}

module Tecs.Editor where

import Prelude as P
import qualified System.IO.UTF8 as UTF8


import Tecs.Types as TT
import Tecs.Text
import Tecs.Buffer
import Tecs.Display

import Control.Monad.State.Lazy

data SimpleEditor = SimpleEditor {
  buffer :: Buffer,
  cursorPos :: Pos,
  fileName :: String
  }

instance Editor SimpleEditor where
  render editor height width = do
    renderBuffer Crop (buffer editor) height width
    setCursor (insertPos editor)
  respond editor evt = execState ((lookupWithDefault evtMap evt) evt) editor

type SimpleEditorAction = State SimpleEditor ()

insertPos :: SimpleEditor -> Pos
insertPos se = let cPos = cursorPos se
                   l = line cPos
                   r = min (row cPos) (length $ lineAt l $ buffer se)
               in Pos l r

advance pos = pos { row = (row pos) + 1}

insertChar :: Char -> SimpleEditorAction
insertChar c = do
  st <- get
  let cursor = insertPos st
      buf = buffer st
  put st { buffer = insertCharIntoBuffer buf cursor c,
           cursorPos = advance cursor }

insertLinebreak :: Event -> SimpleEditorAction
insertLinebreak _ = do
  st <- get
  let cursor = insertPos st
      buf = buffer st
  put $ st { buffer = insertLinebreakIntoBuffer buf cursor,
             cursorPos = Pos { line = (line cursor) + 1,
                                row = 0 } }

cursorDown _ = modify $ \ed ->
  let cp = cursorPos ed
      nextLinePos = min (lastLineIdx $ buffer ed) (line cp + 1)
  in ed { cursorPos = cp { line = nextLinePos }}

cursorUp _ = modify $ \ed ->
  let cp = cursorPos ed
      nextLinePos = max 0 (line cp - 1)
  in ed { cursorPos = cp { line = nextLinePos }}

handleOther evt = case evt of
  KeyEvent (KeyChar c) -> insertChar c
  otherwise -> return ()

evtMap = defaultMapFromList [
  (KeyEvent KeyUp, cursorUp),
  (KeyEvent KeyDown, cursorDown),
  (KeyEvent KeyEnter, insertLinebreak)
  ] handleOther


simpleEditorFromFile :: String -> IO (SimpleEditor)
simpleEditorFromFile filename = do
  s <- UTF8.readFile filename
  return $ SimpleEditor (strToBuffer s) (Pos 0 0) filename

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
