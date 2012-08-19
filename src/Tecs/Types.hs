module Tecs.Types where

import qualified Data.Map as Map


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
  } deriving (Show)

data RenderAction = PrintStr String
                  | SetCursor Pos
                  deriving (Show)

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

data Key = KeyChar Char
         | KeyUp
         | KeyDown
         | KeyLeft
         | KeyRight
           deriving (Show, Eq, Ord)
data Event = KeyEvent Key
           | NoEvent
           deriving (Show, Eq, Ord)

data DefaultMap a b = DefaultMap {
  items :: Map.Map a b,
  defaultValue :: b
  }

defaultMapFromList :: Ord a => [(a, b)] -> b -> DefaultMap a b
defaultMapFromList list defaultVal = DefaultMap (Map.fromList list) defaultVal

lookupWithDefault :: Ord a => DefaultMap a b -> a -> b
lookupWithDefault eMap evt =
  maybe (defaultValue eMap) id (Map.lookup evt (items eMap))

class Editor a where
  render :: a -> Int -> Int -> RenderW ()
  respond :: a -> Event -> a
