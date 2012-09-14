module Tecs.Buffer.LineSeq where

import Tecs.Types
import Tecs.Util (comboBreakers)

import qualified Data.Sequence as Seq
import Data.Char (isSpace)
import Data.Foldable (toList)


isBlank :: String -> Bool
isBlank = all isSpace

isNotBlank :: String -> Bool
isNotBlank = not . isBlank

idxParasAfter :: LineSeq -> LineIdx -> [LineIdx]
idxParasAfter ls idx =
  let idxs = comboBreakers [isNotBlank, isBlank] (Seq.drop idx ls)
  in map (+ idx) idxs

idxParasBefore :: LineSeq -> LineIdx -> [LineIdx]
idxParasBefore ls idx =
  let len = Seq.length ls
      invIdx = len - idx
      idxs = comboBreakers [isNotBlank, isBlank, isNotBlank] (Seq.drop invIdx $ Seq.reverse ls)
  in map (\i -> len - (invIdx + i)) idxs

