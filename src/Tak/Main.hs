{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Control.Monad.State as S
import qualified Data.Map as Map
import System.Environment (getArgs)
import Data.Monoid (mconcat)
import Control.Monad (when)
import Control.Lens

import Tak.Types
import Tak.Display
import Tak.Editor
import Tak.Editor.Cursor (insertPos)
import Tak.Buffer
import Tak.Config (updateInitialPosition)

import Debug.Trace (trace)

topEvtMap :: DefaultMap Event (GlobalState -> IO GlobalState)
topEvtMap =
  let m = Map.fromList [
        (KeyEvent $ KeyCtrlChar 'Q', \st -> do
            let ed = view editor st
            updateInitialPosition (fileName ed) (cursorPos ed)
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
      selStr  = if not $ null (ranges selSt)
                then "[" ++ (show $ fst firstR) ++ "," ++ (show $ snd firstR) ++ "]"
                else ""
      Pos l r = insertPos ed
      posStr  = mconcat [(show (l + 1)), ":", (show r)]
  in mconcat ["  ", modStr, "  ", fn, " ", posStr, " ", selStr]


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

main = do
  args <- getArgs
  startEditor args

startEditor args = do
  if null args
    then putStrLn usage
    else do ed <- simpleEditorFromFile (args !! 0)
            withCurses $ mainLoop $ set editor ed defaultGlobalState
  return ()

