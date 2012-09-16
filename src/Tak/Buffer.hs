{-# LANGUAGE NoImplicitPrelude #-}

module Tak.Buffer where

import Prelude as P
import Data.Sequence ((><), (|>), (<|))
import qualified Data.Sequence as Seq
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Monoid (mconcat)
import Control.Monad (when)

import Tak.Types as TT
import Tak.Util
import Tak.Display
import Tak.Text

import Tak.Buffer.LineSeq
import Tak.Buffer.Line


empty :: Buffer
empty = Buffer (Seq.singleton "")

handleEmptySeq :: Buffer -> Buffer
handleEmptySeq buf =
  if (lineSeq buf) == Seq.empty
  then empty
  else buf

lineSeqToStr :: Seq.Seq String -> String
lineSeqToStr seq = unlines $ toList seq

strToBuffer :: String -> Buffer
strToBuffer s = handleEmptySeq $ Buffer (Seq.fromList (lines s))

bufferToStr :: Buffer -> String
bufferToStr buf = unlines $ bufferToLines buf

bufferToLines :: Buffer -> [Line]
bufferToLines buf = toList (lineSeq buf)

lineAt :: Int -> Buffer -> String
lineAt x buf = Seq.index (lineSeq buf) x


updateLineSeq :: (LineSeq -> LineSeq) -> Buffer -> Buffer
updateLineSeq f buf = buf { lineSeq = f $ lineSeq buf }


bufferDropLines :: Int -> Buffer -> Buffer
bufferDropLines lines buffer =
  handleEmptySeq $ Buffer $ Seq.drop lines (lineSeq buffer)

posWithinBuffer :: Buffer -> Pos -> Pos
posWithinBuffer buf (Pos y x) =
  let l = clamp 0 (lastLineIdx buf) y
      r = clamp 0 (length $ lineAt l buf) x
  in Pos l r



renderLine (idx, str, mHlReg) = do
  let hlRangeStart = maybe (-1) (\(Range (Pos l r) _) -> if l == idx then r else (-1)) mHlReg
      hlRangeEnd   = maybe (-1) (\(Range _ (Pos l r)) -> if l == idx then r else (-1)) mHlReg
      rangeStarts  = hlRangeStart > (-1)
      rangeEnds    = hlRangeEnd   > (-1)
  printStr (Pos idx 0) str
  when (hlRangeStart > (-1)) (invertText >>  (printStr (Pos idx hlRangeStart) (P.drop hlRangeStart str)))
  when (hlRangeEnd   > (-1)) (regularText >> (printStr (Pos idx hlRangeEnd)   (P.drop hlRangeEnd   str)))


renderBuffer :: WrapMode -> Buffer -> Maybe Range -> Int -> Int -> RenderW ()
renderBuffer wrapMode buffer mRange height width =
  let lineStrs = linesToFixedLengthStrs wrapMode width (bufferToLines buffer)
      yPosL = [0..height - 1]
  in do mapM renderLine (P.zip3 yPosL (lineStrs ++ (repeat "")) (repeat mRange))
        return ()


insertCharIntoBuffer :: Buffer -> Pos -> Char -> Buffer
insertCharIntoBuffer buf (Pos y x) char =
  let seq = lineSeq buf
      line = Seq.index seq y
      (before, after) = P.splitAt x line
  in buf { lineSeq = Seq.update y (before ++ [char] ++ after) seq }


deleteCharFromBuffer :: Buffer -> Pos -> Buffer
deleteCharFromBuffer buf (Pos y x) =
  let seq = lineSeq buf
      line = Seq.index seq y
      (before, after) = P.splitAt x line
      newBefore = if not $ P.null before
                  then P.take (P.length before - 1) before
                  else before
  in buf { lineSeq = Seq.update y (newBefore ++ after) seq }

insertLinebreakIntoBuffer :: Buffer -> Pos -> Buffer
insertLinebreakIntoBuffer buf (Pos y x) =
  let seq = lineSeq buf
      (seqBefore, seqRest) = Seq.splitAt y seq
      seqAfter = if Seq.null seqRest
                 then Seq.empty
                 else Seq.drop 1 seqRest
      (before, after) = P.splitAt x (Seq.index seqRest 0)
      seqMiddle = Seq.fromList [before, after]
  in buf { lineSeq = (seqBefore >< seqMiddle >< seqAfter) }

numLines :: Buffer -> Int
numLines buf = Seq.length (lineSeq buf)

lastLineIdx :: Buffer -> Int
lastLineIdx buf = (numLines buf) - 1

deleteLine :: Buffer -> Int -> Buffer
deleteLine buf idx =
  let seq = lineSeq buf
      (left, right) = Seq.splitAt idx seq
  in handleEmptySeq $ buf { lineSeq = (left >< (Seq.drop 1 right)) }

concatLine :: Buffer -> Int -> Buffer
concatLine buf idx =
  let seq = lineSeq buf
      (left, right) = Seq.splitAt (idx - 1) seq
      concatted = (Seq.index right 0) ++ (Seq.index right 1)
  in if idx <= 0 || (Seq.length right) < 2
     then buf
     else buf { lineSeq = (left |> concatted) >< (Seq.drop 2 right) }


cutSelectionLS :: LineSeq -> (Pos, Pos) -> (LineSeq, LineSeq)
cutSelectionLS src ((Pos sl sr), (Pos el er)) =
  let splitOne seq = let (one, rest) = Seq.splitAt 1 seq in (Seq.index one 0, rest)
      rl = (el - sl) - 1
      (before, rem0) = Seq.splitAt sl src
      (startLine, rem1) = splitOne rem0
      (beforeLine, inLine) = P.splitAt sr startLine
      (inLines, _) = Seq.splitAt rl rem1
      (_, rem2) = Seq.splitAt el src
      (lastLine, after) = splitOne rem2
      (outLine, afterLine) = P.splitAt er lastLine
      si = Seq.singleton
  in (if sl == el
         then si (P.take (er - sr) inLine)
         else mconcat [si inLine, inLines, si outLine],
      mconcat [before, si (beforeLine ++ afterLine), after])

cutSelection :: Buffer -> (Pos, Pos) -> (LineSeq, Buffer)
cutSelection buf r =
  let (cut, rest) = cutSelectionLS (lineSeq buf) r
  in (cut, buf { lineSeq = rest })

getSelection :: Buffer -> (Pos, Pos) -> LineSeq
getSelection b r = fst $ cutSelection b r

delSelection :: Buffer -> (Pos, Pos) -> Buffer
delSelection b r = snd $ cutSelection b r

insertLineSeqIntoBuffer :: Buffer -> Pos -> LineSeq -> Buffer
insertLineSeqIntoBuffer buf pos inSeq =
  let Pos l r   = posWithinBuffer buf pos
      seq       = lineSeq buf
      seqBefore = Seq.take l seq
      seqAfter  = Seq.drop (l + 1) seq
      line      = Seq.index seq l
      lineBefore = P.take r line
      lineAfter  = P.drop r line
  in case Seq.length inSeq of
     0 -> buf
     1 -> buf { lineSeq = mconcat [seqBefore,
                                   Seq.singleton (lineBefore ++ (Seq.index inSeq 0) ++ lineAfter),
                                   seqAfter] }
     otherwise ->
         let firstLine = Seq.index inSeq 0
             lastLine  = Seq.index inSeq ((Seq.length inSeq) - 1)
             midLines  = Seq.drop 1 $ Seq.take ((Seq.length inSeq) - 1) inSeq
         in buf { lineSeq = mconcat [seqBefore,
                                     Seq.singleton (lineBefore ++ firstLine),
                                     midLines,
                                     Seq.singleton (lastLine ++ lineAfter),
                                     seqAfter] }

posNextPara :: Buffer -> Pos -> Pos
posNextPara buf pos =
  case idxParasAfter (lineSeq buf) (line pos) of
    idx:_ -> Pos idx 0
    otherwise -> pos

posPrevPara :: Buffer -> Pos -> Pos
posPrevPara buf pos =
  case idxParasBefore (lineSeq buf) (line pos) of
    idx:_ -> Pos idx 0
    otherwise -> pos

posNextWord :: Buffer -> Pos -> Pos
posNextWord buf pos =
  let Pos l r = pos
  in case idxWordsAfter (lineAt l buf) r of
       idx:_ -> Pos l idx
       otherwise -> if l < numLines buf
                    then Pos (l + 1) 0
                    else pos

posPrevWord :: Buffer -> Pos -> Pos
posPrevWord buf pos =
  let Pos l r = pos
  in case idxWordsBefore (lineAt (line pos) buf) (row pos) of
       idx:_ -> Pos (line pos) idx
       otherwise -> if r > 0
                    then Pos l 0
                    else if l > 0
                         then Pos (l - 1) ((P.length $ lineAt (l - 1) buf) - 1)
                         else pos

posFirstPos :: Buffer -> Pos -> Pos
posFirstPos buf _ = idxFirstPos $ lineSeq buf

posLastPos :: Buffer -> Pos -> Pos
posLastPos buf _ = idxLastPos $ lineSeq buf

