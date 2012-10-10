module Tak.RunLoop (doMainLoop, confirm) where

import Tak.Types
import Tak.Display
import Tak.Editor
import Tak.GlobalState
import Control.Lens
import Control.Monad (when, guard)


renderAndRefresh :: GlobalState -> IO ()
renderAndRefresh gst = do
  (y, x) <- getScreenSize
  renderEditor (Box (y - 1) 0       1 x) (view infoLine gst)
  renderEditor (Box 0       0 (y - 1) x) (view editor gst)
  refresh

renderAndWaitEvent :: GlobalState -> IO Event
renderAndWaitEvent gst = do
  when (view needsRepaint gst) (renderAndRefresh gst)
  waitEvent

doMainLoop :: (Event -> GlobalState -> IO a) -> GlobalState -> IO a
doMainLoop handle globalState = do
  evt <- renderAndWaitEvent globalState
  handle evt globalState

confirm :: String -> GlobalState -> IO Bool
confirm msg gst =
  let gst' = updateInfoLine ("    " ++ msg ++ " [y/n] ") gst
      loopConfirm = doMainLoop confirmHandler
      confirmHandler evt gst = case evt of
        KeyEvent (KeyChar 'y') -> return True
        KeyEvent (KeyChar 'n') -> return False
        otherwise              -> loopConfirm gst
  in loopConfirm gst'

