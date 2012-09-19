module Tak.Buffer.LineSeq (idxParasAfter, idxParasBefore, idxFirstPos, idxLastPos, strToLineSeq) where

import Tak.Types
import Tak.Util (comboBreakers)

import qualified Data.Sequence as Seq
import Data.Char (isSpace)
import Data.Foldable (toList)

strToLineSeq :: String -> LineSeq
strToLineSeq s = Seq.fromList $ lines s

isBlank :: String -> Bool
isBlank = all isSpace

isNotBlank :: String -> Bool
isNotBlank = not . isBlank

lastLineIdx :: LineSeq -> LineIdx
lastLineIdx seq = (Seq.length seq) - 1

lastLine seq = Seq.index seq $ lastLineIdx seq

idxParasAfter :: LineSeq -> LineIdx -> [LineIdx]
idxParasAfter ls idx =
  let idxs = comboBreakers [isNotBlank, isBlank] (Seq.drop idx ls)
  in map (+ idx) idxs

idxParasBefore :: LineSeq -> LineIdx -> [LineIdx]
idxParasBefore ls idx =
  let len = Seq.length ls
      invIdx = len - idx
      idxs = comboBreakers [isBlank, isNotBlank] (Seq.drop invIdx $ Seq.reverse ls)
  in map (\i -> len - (invIdx + i)) idxs

idxFirstPos :: LineSeq -> Pos
idxFirstPos _ = Pos 0 0

idxLastPos :: LineSeq -> Pos
idxLastPos seq = Pos (lastLineIdx seq) (length $ lastLine seq)

