module Tak.Buffer.Line where

import Data.Char (isSpace)

import Tak.Types
import Tak.Util (comboBreakers)

isNotSpace = not . isSpace

idxWordsAfter :: Line -> Int -> [Int]
idxWordsAfter l idx = 
  let idxs = comboBreakers [isNotSpace, isSpace] (drop idx l)
  in map (+ idx) idxs


idxWordsBefore :: Line -> Int -> [Int]
idxWordsBefore l idx =
  let len = length l
      invIdx = len - idx
      idxs = comboBreakers [isSpace, isNotSpace] (drop invIdx $ reverse l)
  in map (\i -> len - (invIdx + i)) idxs

