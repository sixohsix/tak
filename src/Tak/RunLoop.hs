module Tak.RunLoop (doMainLoop, confirm, query) where

import Tak.Types
import Tak.Display
import Tak.Editor
import Tak.GlobalState
import Data.Monoid (mappend)
import Control.Lens (set, over, view)
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
        otherwise              -> loopConfirm (preventRepaint gst)
  in loopConfirm gst'


query :: String -> GlobalState -> IO (Maybe String)
query msg =
  let updateIL s = updateInfoLine ("    " ++ msg ++ " " ++ s)
      loopQuery s gst = (doMainLoop $ queryHandler s) (updateIL s gst)
      queryHandler s evt gst =
        case evt of
          KeyEvent (KeyChar c) -> loopQuery (mappend s [c]) gst
          KeyEvent KeyDel      -> loopQuery (reverse $ drop 1 $ reverse s) gst
          KeyEvent KeyEnter    -> return $ Just s
          KeyEvent (KeyCtrlChar 'G') -> return Nothing
          otherwise                  -> loopQuery s gst
  in loopQuery ""

