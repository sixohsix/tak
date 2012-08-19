module Main where

import Control.Monad.State as S
import qualified Data.Map as Map

import Tecs.Types
import Tecs.Display
import Tecs.Editor

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

topEvtMap :: DefaultMap Event (TesState -> Event -> TesState)
topEvtMap =
  let m = Map.fromList [
        (KeyEvent $ KeyChar 'q', \st _ -> st { shouldQuit = True }),
        (KeyEvent KeyEscape, \st _ -> if (inInfoLine st)
                                      then st
                                      else prepareInfoLine st)
        ]
  in DefaultMap m forwardEvtToEditor

handleEvt tesState evt = (lookupWithDefault topEvtMap evt) tesState evt

mainLoop tesState = do
  (y, x) <- getScreenSize
  renderEditor (Box 0       0 (y - 1) x) (editor tesState)
  renderEditor (Box (y - 1) 0      1  x) (infoLine tesState)
  refresh
  evt <- waitEvent
  let nextTesState = handleEvt tesState evt
  if (shouldQuit nextTesState)
    then return ()
    else mainLoop nextTesState

main = withCurses $ do
  editor <- simpleEditorFromFile "./foo.txt"
  mainLoop (defaultTesState editor)
  return ()
