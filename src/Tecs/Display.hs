module Tecs.Display where

import qualified UI.HSCurses.CursesHelper as CH
import qualified UI.HSCurses.Curses as C
import Prelude

import Tecs.Types

out = C.wAddStr C.stdScr

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
