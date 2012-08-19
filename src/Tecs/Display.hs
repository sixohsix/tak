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
  CH.end

clamp :: Int -> Int -> Int -> Int
clamp low high = max low . min high

renderInBox :: ScreenBox -> Pos -> String -> IO ()
renderInBox (ScreenBox (Box lineOffset rowOffset maxHeight maxWidth))
            (Pos line row)
            str =
  let realLine = (clamp 0 maxHeight line) + lineOffset
      realRow  = (clamp 0 maxWidth row) + rowOffset
      realStr  = take (maxWidth - realRow) str
  in do
    C.move realLine realRow
    C.wAddStr C.stdScr str
    return ()

refresh = C.refresh
waitKey = CH.getKey C.refresh

printStr :: String -> RenderW ()
printStr s = RenderW ((), [PrintStr s])
setCursor :: Pos -> RenderW ()
setCursor p = RenderW ((), [SetCursor p])

drawToScreen :: ScreenBox -> RenderAction -> IO ()
drawToScreen (ScreenBox (Box top left height width)) command =
  case command of
    SetCursor (Pos line row) -> C.move (top + (clamp 0 height line))
                                       (left + (clamp 0 width row))
    PrintStr str -> C.wAddStr C.stdScr str
