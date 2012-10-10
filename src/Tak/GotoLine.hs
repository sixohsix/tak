module Tak.GotoLine (gotoLine) where

import Data.Maybe (fromMaybe)
import Control.Lens (set, over, view)

import Tak.Types
import Tak.RunLoop (query)
import Tak.Buffer
import Tak.Editor.Cursor (fixScroll)


gotoLine :: GlobalState -> IO GlobalState
gotoLine gst = do
  lineNoStr <- query "Go to which line?" gst
  return $ case reads $ fromMaybe "" lineNoStr of
             ((lineNo, _):_) -> over editor (\ed -> fixScroll $ ed { cursorPos = posWithinBuffer (buffer ed) (Pos (lineNo - 1) 0) }) gst
             otherwise       -> gst

