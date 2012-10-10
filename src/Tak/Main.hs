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
import Tak.Editor.InfoLine
import Tak.Buffer
import Tak.Config (updateInitialPosition)
import Tak.GlobalState
import Tak.GlobalState.Clipboard (readClipboard, writeClipboard)
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


topEvtMap :: DefaultMap Event (GlobalState -> IO GlobalState)
topEvtMap =
  let m = Map.fromList [
        (KeyEvent $ KeyCtrlChar 'Q', quit),
        (KeyEvent $ KeyCtrlChar 'S', \st -> do
            let ed = activeEditor st
            writeFile (fileName ed) (bufferToStr $ buffer ed)
            return $ over editor (\ed -> ed { lastSavePtr = 0 }) st),
        (KeyEvent $ KeyEscaped $ KeyChar 'P', showKeyEvents),
        (TimeoutEvent, return . preventRepaint)
        ]
  in DefaultMap m (lookupWithDefault editorEvtMap)

handleEvt :: GlobalState -> Event -> IO GlobalState
handleEvt globalState evt = (lookupWithDefault topEvtMap evt) globalState

usage :: String
usage = unlines [
  "USAGE",
  "  tak <file>"
  ]

infoLineContentFor globalState =
  let ed = view editor globalState
      modStr  = if isModified ed
                then "*"
                else " "
      fn      = fileName ed
      selSt   = selState ed
      firstR  = (ranges selSt) !! 0
      Pos l r = insertPos ed
      posStr  = mconcat [(show (l + 1)), ":", (show r)]
  in mconcat ["  ", modStr, "  ", fn, " ", posStr]


topLoop = doMainLoop handler where
  handler evt globalState = do
    nextState <- handleEvt (updateRepaint globalState) evt
    if (view shouldQuit nextState)
      then return ()
      else (updateState >=> topLoop) nextState
  
updateState gst = do
  (y, x) <- getScreenSize
  return $ updateEditorHeight y $ updateInfoLine (infoLineContentFor gst) gst


main = do
  args <- getArgs
  startEditor args

startEditor args = do
  if null args
    then putStrLn usage
    else do ed <- simpleEditorFromFile (args !! 0)
            cb <- readClipboard
            gst <- updateState $ set editor ed $ set clipboard cb $ defaultGlobalState
            withCurses $ topLoop gst
  return ()

