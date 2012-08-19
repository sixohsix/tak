module Main where

import Control.Monad.State as S
import qualified Data.Map as Map

import Tecs.Types
import Tecs.Display
import Tecs.Editor

data TesState = TesState {
  shouldQuit :: Bool,
  editor :: SimpleEditor,
  infoLine :: InfoLineEditor
  }

forwardEvtToEditor tesState evt =
  tesState { editor = respond (editor tesState) evt }

topEvtMap :: DefaultMap Event (TesState -> Event -> TesState)
topEvtMap =
  let m = Map.fromList [
        (KeyEvent $ KeyChar 'q', \st _ -> st { shouldQuit = True })
        ]
  in DefaultMap m forwardEvtToEditor

handleEvt tesState evt = (lookupWithDefault topEvtMap evt) tesState evt

mainLoop tesState = do
  renderEditor (ScreenBox $ Box 0 0 50 50) (editor tesState)
  refresh
  evt <- waitEvent
  let nextTesState = handleEvt tesState evt
  if (shouldQuit nextTesState)
    then return ()
    else mainLoop nextTesState

main = withCurses $ do
  ed <- simpleEditorFromFile "./foo.txt"
  let tesState = TesState False ed defaultInfoLineEditor
  mainLoop tesState
  return ()
