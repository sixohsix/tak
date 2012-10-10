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
import Tak.Buffer
import Tak.Config (updateInitialPosition)
import Tak.GlobalState.Clipboard (readClipboard, writeClipboard)
import Tak.RunLoop

topEvtMap :: DefaultMap Event (GlobalState -> IO GlobalState)
topEvtMap =
  let m = Map.fromList [
        (KeyEvent $ KeyCtrlChar 'Q', \st -> do
            let ed = view editor st
            updateInitialPosition (fileName ed) (cursorPos ed)
            writeClipboard (view clipboard st)
            return $ set shouldQuit True st),
        (KeyEvent $ KeyCtrlChar 'S', \st -> do
            let ed = view editor st
            writeFile (fileName ed) (bufferToStr $ buffer ed)
            return $ over editor (\ed -> ed { lastSavePtr = 0 }) st),
        (TimeoutEvent, return . set needsRepaint False)
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


mainLoop globalState = do
  (y, x) <- getScreenSize
  let infoL        = view infoLine globalState
      globalState' = (over editor (\ed -> ed { viewHeight = y - 1 }))
                       $ (over infoLine (\il -> setInfoLineContent il $ infoLineContentFor globalState) globalState)
  evt <- renderAndWaitEvent globalState'
  nextGlobalState <- handleEvt (set needsRepaint True globalState') evt
  if (view shouldQuit nextGlobalState)
    then return ()
    else mainLoop nextGlobalState

updateEditorHeight y = (over editor (\ed -> ed { viewHeight = y - 1 }))
updateInfoLine str = over infoLine (\il -> setInfoLineContent il str)

topLoop = doMainLoop handler where
  handler evt globalState = do
    nextState <- handleEvt  globalState evt
    if (view shouldQuit nextState)
      then return ()
      else (updateState >=> topLoop) nextState
  updateState st = do
    (y, x) <- getScreenSize
    return $ updateRepaint $ updateEditorHeight y $ updateInfoLine (infoLineContentFor st) st

main = do
  args <- getArgs
  startEditor args

startEditor args = do
  if null args
    then putStrLn usage
    else do ed <- simpleEditorFromFile (args !! 0)
            cb <- readClipboard
            withCurses $ mainLoop $ set editor ed $ set clipboard cb $ defaultGlobalState
  return ()

