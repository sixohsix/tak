{-# LANGUAGE NoImplicitPrelude #-}

module Tecs.Buffer where

import Prelude as P
import Tecs.Types as TT
import Tecs.Display
import Tecs.Text

bufferFromStr :: String -> Buffer
bufferFromStr s = Buffer _lines where
  _lines :: [Line]
  _lines = do
    line <- P.lines s
    return (Line line)


renderBuffer :: WrapMode -> Buffer -> Int -> Int -> RenderW ()
renderBuffer wrapMode buffer height width =
  let lineStrs = linesToFixedLengthStrs wrapMode width (TT.lines buffer)
      yPosL = [0..height - 1]
      p (yPos, str) = do
        setCursor (Pos yPos 0)
        printStr str
  in do mapM p (zip yPosL lineStrs)
        return ()
