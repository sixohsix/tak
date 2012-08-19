{-# LANGUAGE NoImplicitPrelude #-}

module Tecs.Editor where

import Prelude

import Tecs.Types as TT
import Tecs.Text
import Tecs.Buffer
import Tecs.Display

data SimpleEditor = SimpleEditor {
  buffer :: Buffer,
  cursorPos :: Pos
  }

instance Editor SimpleEditor where
  render editor height width =
    let lineStrs = linesToFixedLengthStrs Crop width (TT.lines $ buffer editor)
        yPosL = [0..height - 1]
        p (yPos, str)= do
          setCursor (Pos yPos 0)
          printStr str
    in do mapM p (zip yPosL lineStrs)
          setCursor (cursorPos editor)


simpleEditorFromFile filename = do
  s <- readFile filename
  return $ SimpleEditor (bufferFromStr s) (Pos 0 0)


renderEditor sb@(ScreenBox (Box _ _ height width)) editor =
  let (_, commands) = execRender (render editor height width)
  in do mapM (drawToScreen sb) commands
        return ()
