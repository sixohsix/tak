module Tecs.Buffer.LineSeq where

import Tecs.Types

import qualified Data.Sequence as Seq
import Data.Char (isSpace)
import Data.Foldable (toList)


isBlank :: String -> Bool
isBlank = all isSpace

isNotBlank :: String -> Bool
isNotBlank = not . isBlank

comboBreakers :: [(String -> Bool)] -> LineSeq -> [LineIdx]
comboBreakers fs lSeq =
  cBreakers fs (zip [0..] (toList lSeq))
  where
    cBreakers _ [] = []
    cBreakers [] ((idx, _):rest) = idx:(cBreakers fs rest)
    cBreakers (f:ff) ((idx, line):rest) = case f line of
      True  -> cBreakers (f:ff) rest
      False -> cBreakers ff ((idx, line):rest)

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

