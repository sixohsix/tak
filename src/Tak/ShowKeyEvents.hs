module Tak.ShowKeyEvents (showKeyEvents) where

import Tak.Types
import Tak.RunLoop
import Tak.GlobalState


keyMessage = "Showing key events. Use 'q' to quit:  "

showKeyEventLoop = doMainLoop handleKeyEvents

showKeyEvents :: GlobalState -> IO GlobalState
showKeyEvents = showKeyEventLoop . updateInfoLine keyMessage


handleKeyEvents evt gst =
  case evt of
    KeyEvent (KeyChar 'q') -> return gst
    TimeoutEvent -> showKeyEventLoop gst
    otherwise -> showKeyEventLoop $ updateInfoLine (keyMessage ++ (show evt)) gst

