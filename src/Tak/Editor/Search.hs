module Tak.Editor.Search (search) where

import Tak.Types
import Tak.RunLoop
import Tak.Util
import Tak.Buffer (cutSelectionLS)
import Tak.GlobalState
import Tak.Editor.Cursor (fixScroll)
import Tak.Editor.Selection (startSelecting)

import Control.Lens (set, over, view)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.List (length)
import Data.Foldable (toList)
import qualified Data.Text as T
import qualified Data.Sequence as S

search :: GlobalState -> IO GlobalState
search = searchLoop "" . (over editor startSelecting)

searchLoop sstr = doMainLoop (handler sstr) . updateRepaint

lsAfter pos ls = snd $ cutSelectionLS ls (Pos 0 0, pos)

findStringsAfter :: String -> Pos -> LineSeq -> [Pos]
findStringsAfter [] _ _ = []
findStringsAfter sstr pos ls = 
  fmap (moveRelative pos) $ mconcat $ zipWith f (toList $ lsAfter pos ls) [0..] where
    f line idx = fmap (g idx) $ T.breakOnAll (T.pack sstr) line
    g idx (before, _) = Pos idx (T.length before)

updateSelectionWithNextFind :: String -> Pos -> GlobalState -> GlobalState
updateSelectionWithNextFind sstr pos gst =
  let ed = view editor gst
      ls = lineSeq $ buffer ed
      slen = length sstr
      stringsAfter = findStringsAfter sstr pos ls
      startPos:_ = stringsAfter
      endPos = shift startPos (Pos 0 slen)
  in case stringsAfter of
       []      -> gst
       (p:_)   -> over editor (\ed -> fixScroll $ ed { cursorPos = endPos, selState = (selState ed) { openRange = Just startPos } }) gst

handler sstr evt gst = 
  let loopNextSstr s p = searchLoop s . updateSelectionWithNextFind s p
      curPos = cursorPos (view editor gst)
      pos = fromMaybe curPos (openRange $ selState (view editor gst))
  in case evt of
       KeyEvent (KeyChar c) -> loopNextSstr (sstr ++ [c]) pos gst
       KeyEvent KeyDel      -> loopNextSstr (reverse $ drop 1 $ reverse sstr) pos gst
       KeyEvent (KeyCtrlChar 'G') -> return gst
       KeyEvent KeyEnter          -> return gst
       KeyEvent (KeyCtrlChar 'N') -> loopNextSstr sstr curPos gst
       otherwise -> searchLoop sstr $ preventRepaint gst

