{-# LANGUAGE TemplateHaskell #-}

module Tak.Types where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Control.Monad.State as St
import qualified Data.Text as Text
import Control.Lens (makeLenses)

type Line = Text.Text

type LineIdx = Int

type LineSeq = Seq.Seq Line

data Buffer = Buffer {
  lineSeq :: LineSeq
  } deriving (Show)
defaultBuffer = Buffer $ Seq.fromList [Text.empty]

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
         | KeyEscaped Key
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

data Mode = Mode {
  handler :: Event -> GlobalState -> IO GlobalState
}

defaultMode = Mode (\e g -> return g)

data GlobalState = GlobalState {
  _shouldQuit :: Bool,
  _needsRepaint :: Bool,
  _clipboard :: [LineSeq],
  _editor :: SimpleEditor,
  _infoLine :: InfoLineEditor,
  _mode :: Mode
  }

makeLenses ''GlobalState

defaultGlobalState =
  GlobalState False True [] defaultSimpleEditor defaultInfoLineEditor defaultMode

