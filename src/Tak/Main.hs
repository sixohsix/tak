{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Control.Monad.State as S
import qualified Data.Map as Map
import System.Environment (getArgs)
import Data.Monoid (mconcat)
import Control.Monad (when, (>=>) )
import Control.Lens

import Tak.Types
import Tak.Display
import Tak.Editor
import Tak.Editor.Cursor (insertPos)
import Tak.Editor.Search
import Tak.Editor.InfoLine
import Tak.Buffer
import Tak.Config (updateInitialPosition)
import Tak.GlobalState
import Tak.GlobalState.Clipboard (readClipboard, writeClipboard)
import Tak.GotoLine (gotoLine)
import Tak.RunLoop
import Tak.ShowKeyEvents


quit gst = do
  let ed = activeEditor gst
      unsavedChanges = (lastSavePtr ed) /= 0
  shouldQuit <- if unsavedChanges
                then confirm "There are unsaved changes. Are you sure you want to quit?" gst
                else return True
  nextState <- if shouldQuit
               then quitRightAway gst
               else return gst
  return nextState


quitRightAway gst = do
  let ed = activeEditor gst
  updateInitialPosition (fileName ed) (cursorPos ed)
  writeClipboard (view clipboard gst)
  return $ set shouldQuit True gst


topEvtMap :: Map.Map Event (GlobalState -> IO GlobalState)
topEvtMap = Map.fromList [
  (KeyEvent $ KeyCtrlChar 'Q', quit),
  (KeyEvent $ KeyCtrlChar 'S', \st -> do
     let ed = activeEditor st
     writeFile (fileName ed) (bufferToStr $ buffer ed)
     return $ over editor (\ed -> ed { lastSavePtr = 0 }) st),
  (KeyEvent $ KeyEscaped $ KeyChar 'P', showKeyEvents),
  (KeyEvent $ KeyEscaped $ KeyChar 'G', gotoLine),
  (KeyEvent $ KeyEscaped $ KeyChar 's', search),
  (TimeoutEvent, return . preventRepaint)
  ]


handleEvt :: Event -> GlobalState -> IO GlobalState
handleEvt evt gst = 
  let modeHandler = (handler $ view mode gst) evt
  in (Map.findWithDefault modeHandler evt topEvtMap) gst


usage :: String
usage = unlines [
  "USAGE",
  "  tak <file>"
  ]

topLoop = doMainLoop handler where
  handler evt globalState = do
    nextState <- handleEvt evt globalState
    if (view shouldQuit nextState)
      then return ()
      else (updateState >=> topLoop) nextState
  
updateState gst = do
  (y, x) <- getScreenSize
  return $ updateEditorHeight y $ updateInfoLine (infoLineContent gst Nothing) gst

updateInfoLineInitMsg = updateInfoLineDefaultMsg "Welcome to Tak. Type C-q to quit"

main = do
  args <- getArgs
  startEditor args

startEditor args = do
  if null args
    then putStrLn usage
    else do ed <- simpleEditorFromFile (args !! 0)
            cb <- readClipboard
            gst <- updateState $ set editor ed $ set clipboard cb $ set mode defaultEditorMode $ defaultGlobalState
            withCurses $ topLoop (updateInfoLineInitMsg gst)
  return ()

