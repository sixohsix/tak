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

data RenderAction = PrintStr String
                  | SetCursor Pos
type RenderActions = [RenderAction]
newtype RenderW a = RenderW {
  execRender :: (a, RenderActions)
  }
instance Monad RenderW where
  return x = RenderW (x, [])
  m >>= k = let (a, ras)  = execRender m
                n         = k a
                (b, ras') = execRender n
            in RenderW (b, ras ++ ras')

class Editor a where
  render :: a -> Int -> Int -> RenderW ()
