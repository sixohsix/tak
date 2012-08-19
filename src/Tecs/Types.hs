module Tecs.Types where

data Line = Line String
data Buffer = Buffer {
  lines :: [Line]
  }

data Box = Box {
  top :: Int,
  left :: Int,
  height :: Int,
  width :: Int
  }

bottom :: Box -> Int
bottom box = top box + height box

right :: Box -> Int
right box = left box + width box

data ScreenBox = ScreenBox Box

data WrapMode = Crop
              | Wrap

data BufferView = BufferView {
  box :: Box,
  wrapMode :: WrapMode
  }

data Pos = Pos {
  line :: Int,
  row :: Int
  }

data CursorPos = CursorPos Pos
data BufferPos = BufferPos Pos
data ScreenPos = ScreenPos Pos

class Editor a where
  render :: a -> Int -> Int -> (Pos -> String -> IO ()) -> IO ()
