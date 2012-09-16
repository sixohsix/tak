module Tak.Util where

import Tak.Types
import Data.Foldable (Foldable, toList)


clamp :: Int -> Int -> Int -> Int
clamp low high = max low . min high


comboBreakers :: Foldable t => [(a -> Bool)] -> t a -> [Int]
comboBreakers fs lSeq =
  cBreakers fs (zip [0..] (toList lSeq))
  where
    cBreakers _ [] = []
    cBreakers [] ((idx, _):rest) = idx:(cBreakers fs rest)
    cBreakers (f:ff) ((idx, line):rest) = case f line of
      True  -> cBreakers (f:ff) rest
      False -> cBreakers ff ((idx, line):rest)


moveRelative :: Pos -> Pos -> Pos
moveRelative (Pos ol or) (Pos dl dr) =
  if dl == 1
     then Pos ol (or + dr)
     else Pos (ol + dl) dr


shift :: Pos -> Pos -> Pos
shift (Pos l0 r0) (Pos l1 r1) = Pos (l0 + l1) (r0 + r1)

