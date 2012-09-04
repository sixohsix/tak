{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Control.Monad.State as S
import qualified Data.Map as Map
import System.Environment (getArgs)

import Tecs.Types
import Tecs.Display
import Tecs.Editor
import Tecs.Buffer

import Debug.Trace (trace)

data TesState = TesState {
  shouldQuit :: Bool,
  inInfoLine :: Bool,
  editor :: SimpleEditor,
  infoLine :: InfoLineEditor
  }
defaultTesState editor =
  TesState False False editor defaultInfoLineEditor

forwardEvtToEditor tesState evt =
  if (inInfoLine tesState)
  then tesState { infoLine = respond (infoLine tesState) evt }
  else tesState { editor = respond (editor tesState) evt }

prepareInfoLine tesState = tesState {
  inInfoLine = True,
  infoLine = setInfoLineContent (infoLine tesState ) ":"
  }

topEvtMap :: DefaultMap Event (TesState -> Event -> IO TesState)
topEvtMap =
  let m = Map.fromList [
        (KeyEvent $ KeyCtrlChar 'Q', \st _ -> return $ st { shouldQuit = True }),
        (KeyEvent $ KeyCtrlChar 'S', \st _ -> do
            let ed = editor st
            writeFile (fileName ed) (bufferToStr $ buffer ed)
            return st { editor = ed { lastSavePtr = 0 } }
            )
        ]
  in DefaultMap m (\ts ev -> do let ts' = forwardEvtToEditor ts ev
                                return ts'
                  )

handleEvt :: TesState -> Event -> IO TesState
handleEvt tesState evt = (lookupWithDefault topEvtMap evt) tesState evt

usage :: String
usage = unlines [
  "USAGE",
  "  tes <file>"
  ]

infoLineContentFor tesState =
  let ed = editor tesState
      modStr = if isModified ed
               then "*"
               else " "
      fn     = fileName ed
  in "  " ++ modStr ++ "  " ++ fn

mainLoop tesState = do
  (y, x) <- getScreenSize
  let infoL     = infoLine tesState
      tesState' = tesState { editor = (editor tesState) { viewHeight = y - 1 },
                             infoLine = setInfoLineContent infoL (infoLineContentFor tesState) }
  renderEditor (Box (y - 1) 0      1  x) (infoLine tesState')
  renderEditor (Box 0       0 (y - 2) x) (editor tesState')
  refresh
  evt <- waitEvent
  nextTesState <- handleEvt tesState' evt
  if (shouldQuit nextTesState)
    then return ()
    else mainLoop nextTesState

main = do
  args <- getArgs
  if null args
    then putStrLn usage
    else do editor <- simpleEditorFromFile (args !! 0)
            withCurses $ mainLoop (defaultTesState editor)
  return ()
