{-# LANGUAGE NoImplicitPrelude #-}

module Tecs.Editor where

import Prelude

import Tecs.Types as TT
import Tecs.Text
import Tecs.Buffer
import Tecs.Display

data SimpleEditor = SimpleEditor {
  buffer :: Buffer
  }

instance Editor SimpleEditor where
  render editor height width render' =
    let lineStrs = linesToFixedLengthStrs Crop width (TT.lines $ buffer editor)
        yPosL = [0..height - 1]
    in do mapM (\(yPos, str) -> render' (Pos yPos 0) str) (zip yPosL lineStrs)
          return ()


simpleEditorFromFile filename = do
  s <- readFile filename
  return $ SimpleEditor (bufferFromStr s)


renderEditor sb@(ScreenBox (Box _ _ height width)) editor =
  render editor height width (renderInBox sb)
