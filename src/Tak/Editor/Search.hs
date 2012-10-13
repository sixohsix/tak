module Tak.Editor.Search (search) where

import Tak.Types
import Tak.RunLoop
import Tak.Util
import Tak.Buffer (cutSelectionLS)
import Tak.GlobalState
import Tak.Editor.Cursor (fixScrollCentered)
import Tak.Editor.Selection (startSelecting)
import Tak.Editor.InfoLine (infoLineContent)

import Control.Lens (set, over, view)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.List (length)
import Data.Foldable (toList)
import qualified Data.Text as T
import qualified Data.Sequence as S


search :: GlobalState -> IO GlobalState
search gst = searchLoop "" $ (updateInfoLine $ infoLineContent gst $ Just "Start typing to search") gst


searchLoop sstr = doMainLoop (handler sstr)

lsAfter pos ls = snd $ cutSelectionLS ls (Pos 0 0, pos)

findStringsAfter :: String -> Pos -> LineSeq -> [Pos]
findStringsAfter [] _ _ = []
findStringsAfter sstr pos ls = 
  fmap (moveRelative pos) $ mconcat $ zipWith f (toList $ lsAfter pos ls) [0..] where
    f line idx = fmap (g idx) $ T.breakOnAll (T.pack sstr) line
    g idx (before, _) = Pos idx (T.length before)

updateInfoLineSearching sstr wasFound gst =
  let searching   = ["Searching for: '", sstr, "'", wasFoundStr]
      wasFoundStr = if wasFound then "" else " NOT FOUND"
  in updateInfoLine (infoLineContent gst $ Just $ mconcat searching) gst

updateSelectionWithNextFind :: String -> Pos -> GlobalState -> GlobalState
updateSelectionWithNextFind sstr pos gst =
  let ed = view editor gst
      ls = lineSeq $ buffer ed
      slen = length sstr
      stringsAfter = findStringsAfter sstr pos ls
      startPos:_ = stringsAfter
      endPos = shift startPos (Pos 0 slen)
      wasFound = stringsAfter /= []
      updateIL = updateInfoLineSearching sstr wasFound
  in updateIL $ case stringsAfter of
       []      -> gst
       (p:_)   -> over editor (\ed -> fixScrollCentered $ ed { cursorPos = endPos, selState = (selState ed) { openRange = Just startPos } }) gst

handler sstr evt gst = 
  let loopNextSstr s p = searchLoop s . updateSelectionWithNextFind s p
      curPos = cursorPos (view editor gst)
      pos = fromMaybe curPos (openRange $ selState (view editor gst))
  in case evt of
       KeyEvent (KeyChar c) -> loopNextSstr (sstr ++ [c]) pos gst
       KeyEvent KeyDel      -> loopNextSstr (reverse $ drop 1 $ reverse sstr) pos gst
       KeyEvent (KeyEscaped (KeyChar 's')) -> loopNextSstr sstr curPos gst
       KeyEvent _           -> return gst
       otherwise            -> searchLoop sstr $ preventRepaint gst

