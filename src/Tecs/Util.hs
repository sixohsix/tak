module Tecs.Util where

import Tecs.Types
import Data.Foldable (Foldable, toList)


clamp :: Int -> Int -> Int -> Int
clamp low high = max low . min high

comboBreakers :: Foldable t => [(a -> Bool)] -> t a -> [Int]
comboBreakers fs lSeq =
  cBreakers fs (zip [0..] (toList lSeq)) []
  where
    cBreakers _ [] acc = acc
    cBreakers [] ((idx, _):rest) acc = cBreakers fs rest (acc ++ [idx])
    cBreakers (f:ff) ((idx, line):rest) acc = case f line of
      True  -> cBreakers (f:ff) rest acc
      False -> cBreakers ff ((idx, line):rest) acc

