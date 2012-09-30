{-# LANGUAGE TemplateHaskell #-}

module Tak.Types where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Control.Monad.State as St
import Control.Lens (makeLenses)

type Line = String

type LineIdx = Int

type LineSeq = Seq.Seq Line

data Buffer = Buffer {
  lineSeq :: LineSeq
  } deriving (Show)
defaultBuffer = Buffer $ Seq.fromList [""]

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
  } deriving (Show, Eq, Ord)
defaultPos :: Pos
defaultPos = Pos 0 0

data Range = Range {
  start :: Pos,
  end :: Pos
  }

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
         | KeyEscapedChar Char
         | KeyUp
         | KeyDown
         | KeyLeft
         | KeyRight
         | KeyCtrlUp
         | KeyCtrlDown
         | KeyCtrlLeft
         | KeyCtrlRight
         | KeyEscape
         | KeyEnter
         | KeyDel
         | KeyPageDown
         | KeyPageUp
         | KeyHome
         | KeyEnd
         | KeyCtrlHome
         | KeyCtrlEnd
           deriving (Show, Eq, Ord)
data Event = KeyEvent Key
           | TimeoutEvent
           | NoEvent
           deriving (Show, Eq, Ord)

data DefaultMap k v = DefaultMap {
  items :: Map.Map k v,
  defaultValue :: k -> v
  }

defaultMapFromList :: Ord a => [(a, b)] -> (a -> b) -> DefaultMap a b
defaultMapFromList list defaultVal =
  DefaultMap (Map.fromList list) defaultVal

lookupWithDefault :: Ord a => DefaultMap a b -> a -> b
lookupWithDefault dMap k =
  maybe (defaultValue dMap $ k) id (Map.lookup k (items dMap))

class Editor a where
  render :: a -> Int -> Int -> RenderW ()

data SelectionState = SelectionState {
  ranges :: [(Pos, Pos)],
  openRange :: Maybe Pos
  }
defaultSelectionState = SelectionState [] Nothing

data SimpleEditor = SimpleEditor {
  undoBuffers :: [(Buffer, Pos)],
  lastSavePtr :: Int,
  selState :: SelectionState,
  buffer :: Buffer,
  cursorPos :: Pos,
  fileName :: String,
  lineScroll :: Int,
  viewHeight :: Int
  }
defaultSimpleEditor :: SimpleEditor
defaultSimpleEditor =
  SimpleEditor [] 0 defaultSelectionState defaultBuffer defaultPos "" 0 24

data InfoLineEditor = InfoLineEditor {
  infoBuffer :: Buffer
  }
defaultInfoLineEditor = InfoLineEditor defaultBuffer

data GlobalState = GlobalState {
  _shouldQuit :: Bool,
  _needsRepaint :: Bool,
  _clipboard :: [Seq.Seq String],
  _editor :: SimpleEditor,
  _infoLine :: InfoLineEditor
  }

makeLenses ''GlobalState

defaultGlobalState =
  GlobalState False True [] defaultSimpleEditor defaultInfoLineEditor

