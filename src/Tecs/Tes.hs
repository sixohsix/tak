module Main where

import Tecs.Types
import Tecs.Display
import Tecs.Editor

main = withCurses $ do
  ed <- simpleEditorFromFile "./foo.txt"
  renderEditor (ScreenBox $ Box 0 0 20 20) ed
  refresh
  waitKey
  return ()
