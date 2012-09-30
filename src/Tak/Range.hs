module Tak.Range (makeRange, asTuple, shiftRange, startPos, endPos) where

import Tak.Types
import Tak.Util


makeRange :: Pos -> Pos -> Range
makeRange p0 p1 =
  Range (min p0 p1) (max p0 p1)


asTuple :: Range -> (Pos, Pos)
asTuple r = let Range p0 p1 = r in (p0, p1)


shiftRange :: Range -> Pos -> Range
(Range p0 p1) `shiftRange` p = Range (p0 `shift` p) (p1 `shift` p)


startPos :: Range -> Pos
startPos (Range s e) = s


endPos :: Range -> Pos
endPos (Range s e) = e

