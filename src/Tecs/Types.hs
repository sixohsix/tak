module Tecs.Types where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Control.Monad.State as St


data Buffer = Buffer {
  lineSeq :: Seq.Seq String
  } deriving (Show)

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

data WrapMode = Crop
              | Wrap

data Pos = Pos {
  line :: Int,
  row :: Int
  } deriving (Show)

data RenderAction = PrintStr Pos String
                  | SetCursor Pos
                  | SetColorPair Int
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
         | KeyCtrlChar Char
         | KeyUp
         | KeyDown
         | KeyLeft
         | KeyRight
         | KeyEscape
         | KeyEnter
         | KeyDel
         | KeyPageDown
         | KeyPageUp
           deriving (Show, Eq, Ord)
data Event = KeyEvent Key
           | NoEvent
           deriving (Show, Eq, Ord)

data DefaultMap a b = DefaultMap {
  items :: Map.Map a b,
  defaultValue :: b
  }

defaultMapFromList :: Ord a => [(a, b)] -> b -> DefaultMap a b
defaultMapFromList list defaultVal =
  DefaultMap (Map.fromList list) defaultVal

lookupWithDefault :: Ord a => DefaultMap a b -> a -> b
lookupWithDefault eMap evt =
  maybe (defaultValue eMap) id (Map.lookup evt (items eMap))

class Editor a where
  render :: a -> Int -> Int -> RenderW ()
  respond :: a -> Event -> a

data Ring a = Ring {
  members :: [a],
  idx :: Int
  }
defaultRing = Ring [] 0

type BufferRing = Ring Buffer
