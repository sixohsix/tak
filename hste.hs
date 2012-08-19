{-# LANGUAGE NoImplicitPrelude #-}

module Hste where

import qualified UI.HSCurses.CursesHelper as CH
import qualified UI.HSCurses.Curses as C
import Data.List (foldl')
import Prelude


out = C.wAddStr C.stdScr

withCurses :: IO () -> IO ()
withCurses f = do
  CH.start
  C.echo False
  f
  CH.end


data Buffer = LineBuffer [Line]
data Line = Line String


bufferFromStr :: String -> Buffer
bufferFromStr s = LineBuffer _lines where
  _lines :: [Line]
  _lines = do
    line <- lines s
    return (Line line)

data Box = Box {
  top :: Int,
  left :: Int,
  height :: Int,
  width :: Int
  }
bottom box = top box + height box
right box = left box + width box

data WrapMode = Crop
              | Wrap

data BufferView = BufferView {
  box :: Box,
  wrapMode :: WrapMode
  }

outBuffer :: Box -> Buffer -> IO ()
outBuffer box buf =
  let LineBuffer lines = buf
      yposL = [(top box)..(bottom box) - 1]
      indent = left box
      outLine :: Int -> Line -> IO ()
      outLine ypos (Line line) = do
        C.move ypos indent
        out line
  in do mapM (\(ypos, line) -> outLine ypos line) (zip yposL lines)
        return ()


data Pos = Pos {
  line :: Int,
  row :: Int
  }

data ScreenOffset = ScreenOffset Pos

clamp :: Int -> Int -> Int -> Int
clamp low high = max low . min high

isInBox :: Box -> Pos -> Bool
isInBox box pos =
  ((line pos) >= (top box)) && ((line pos) < (bottom box))
    && ((row pos) >= (left box)) && ((row pos) < (right box))

fitToBox :: Box -> Pos -> Pos
fitToBox box pos =
  Pos (clamp (top box) (bottom box) (line pos))
      (clamp (left box) (right box) (row pos))

data CursorPos = CursorPos Pos
data BufferPos = BufferPos Pos
data ScreenPos = ScreenPos Pos

posInBuffer :: Buffer -> CursorPos -> BufferPos
posInBuffer (LineBuffer lines) (CursorPos (Pos l r)) =
  let linePos = min l (length lines)
      Line lineStr = (lines !! linePos)
      rowPos = min r (length lineStr)
  in BufferPos $ Pos linePos rowPos

isInBufferView (BufferView box _) pos = isInBox box pos


wrapString :: Int -> String -> [String]
wrapString _ "" = [""]
wrapString width str = wrapString' width str
wrapString' _ "" = []
wrapString' width str =
  let (left, right) = Prelude.splitAt width str
  in left:(wrapString' width right)


linesToFixedLengthStrs :: WrapMode -> Int -> [Line] -> [String]
linesToFixedLengthStrs wrapMode width lines =
  let lineToStrs (Line line) = case wrapMode of
        Crop      -> [take width line]
        otherwise -> wrapString width line
  in concat $ map lineToStrs lines


drawBufferView (LineBuffer lines) (BufferView box wrapMode) (ScreenOffset (Pos sl sr)) =
  let rowPos = sr
      linePosL = [sl..(height box) + sl - 1]
      linesStart = drop (top box) lines
      lineStrs = linesToFixedLengthStrs wrapMode (width box) linesStart
      outStr (linePos, s) = do
        C.move linePos rowPos
        out s
  in do mapM outStr (zip linePosL lineStrs)
        return ()


main = withCurses $ do
  s <- readFile "./foo.txt"
  let buf = bufferFromStr s
  drawBufferView buf (BufferView (Box 0 0 3 6) Wrap) (ScreenOffset (Pos 4 4))
  C.refresh
  CH.getKey C.refresh
  return ()
