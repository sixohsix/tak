module Tecs.Display where

import qualified UI.HSCurses.CursesHelper as CH
import qualified UI.HSCurses.Curses as C
import Prelude
import Control.Monad.Writer

import Tecs.Types

withCurses :: IO () -> IO ()
withCurses f = do
  CH.start
  C.echo False
  f
  C.endWin
  CH.end

getScreenSize :: IO (Int, Int)
getScreenSize = C.scrSize

clamp :: Int -> Int -> Int -> Int
clamp low high = max low . min high

refresh = C.refresh
waitKey = CH.getKey C.refresh

cursesKeyToEvt :: C.Key -> Event
cursesKeyToEvt (C.KeyChar '\ESC') = KeyEvent KeyEscape
cursesKeyToEvt (C.KeyChar c) = KeyEvent $ KeyChar c
cursesKeyToEvt C.KeyUp       = KeyEvent KeyUp
cursesKeyToEvt C.KeyDown     = KeyEvent KeyDown
cursesKeyToEvt _             = NoEvent

waitEvent :: IO (Event)
waitEvent = do
  key <- waitKey
  return $ cursesKeyToEvt key

printStr :: String -> RenderW ()
printStr s = RenderW ((), [PrintStr s])
setCursor :: Pos -> RenderW ()
setCursor p = RenderW ((), [SetCursor p])

drawToScreen :: Box -> RenderAction -> IO ()
drawToScreen (Box top left height width) command =
  case command of
    SetCursor (Pos line row) -> C.move (top + (clamp 0 height line))
                                       (left + (clamp 0 width row))
    PrintStr str -> C.wAddStr C.stdScr str
