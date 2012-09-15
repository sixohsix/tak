
import Tak.Display
import UI.HSCurses.Curses
import Data.Char

main = withCurses mainLoop

mainLoop = do
  move 0 0
  c <- getch
  print c
  print $ show $ decodeKey c
  if toInteger c == (toInteger $ ord 'q')
    then return ()
    else mainLoop
