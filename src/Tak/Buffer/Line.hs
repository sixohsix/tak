module Tak.Buffer.Line where

import Data.Char (isSpace, ord)
import qualified Data.Text as Text

import Tak.Types
import Tak.Util (comboBreakers)

isNotSpace = not . isSpace

idxWordsAfter :: Line -> Int -> [Int]
idxWordsAfter l idx = 
  let idxs = comboBreakers [isNotSpace, isSpace] (Text.unpack $ Text.drop idx l)
  in map (+ idx) idxs


idxWordsBefore :: Line -> Int -> [Int]
idxWordsBefore l idx =
  let len = Text.length l
      invIdx = len - idx
      idxs = comboBreakers [isSpace, isNotSpace] (Text.unpack $ Text.drop invIdx $ Text.reverse l)
  in map (\i -> len - (invIdx + i)) idxs


length = Text.length
take = Text.take
drop = Text.drop


unweird :: Char -> String
unweird '\t' = "        "
unweird a = [a]


charWidth :: Char -> Int
charWidth c
  | c == '\t'   = 8
  | ord c < 255 = 1
  | otherwise   = 2


lineWidth :: Line -> Int
lineWidth line = sum $ map charWidth $ Text.unpack line

