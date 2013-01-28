module Tak.GlobalState where

import Control.Lens

import Tak.Types
import Tak.Editor.InfoLine

activeEditor :: GlobalState -> SimpleEditor
activeEditor = view editor

pasteable gst = case (view clipboard gst) of
  x:_       -> Just x
  otherwise -> Nothing


updateEditorHeight y = (over editor (\ed -> ed { viewHeight = y - 1 }))

updateInfoLine str = over infoLine (\il -> setInfoLineContent il str)

updateInfoLineDefault gst = (updateInfoLine $ infoLineContent gst Nothing) gst
updateInfoLineDefaultMsg msg gst = (updateInfoLine $ infoLineContent gst $ Just msg) gst

updateRepaint :: GlobalState -> GlobalState
updateRepaint = set needsRepaint True

preventRepaint = set needsRepaint False

