module Tecs.Util where

import Tecs.Types
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

