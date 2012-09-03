{-# LANGUAGE NoImplicitPrelude #-}

module Tecs.Buffer where

import Prelude as P
import Data.Sequence as Seq
import Data.Foldable (toList)

import Tecs.Types as TT
import Tecs.Display
import Tecs.Text

empty :: Buffer
empty = Buffer (Seq.fromList [])

strToBuffer :: String -> Buffer
strToBuffer s = Buffer (Seq.fromList (lines s))

bufferToStr :: Buffer -> String
bufferToStr buf = unlines $ bufferToLines buf

bufferToLines :: Buffer -> [String]
bufferToLines buf = toList (lineSeq buf)

lineAt :: Int -> Buffer -> String
lineAt x buf = Seq.index (lineSeq buf) x

bufferDropLines :: Int -> Buffer -> Buffer
bufferDropLines lines buffer =
  Buffer $ Seq.drop lines (lineSeq buffer)

renderBuffer :: WrapMode -> Buffer -> Int -> Int -> RenderW ()
renderBuffer wrapMode buffer height width =
  let lineStrs = linesToFixedLengthStrs wrapMode width (bufferToLines buffer)
      yPosL = [0..height - 1]
      p (yPos, str) = printStr (Pos yPos 0) str
  in do mapM p (P.zip yPosL lineStrs)
        return ()


insertCharIntoBuffer :: Buffer -> Pos -> Char -> Buffer
insertCharIntoBuffer buf (Pos y x) char =
  let seq = lineSeq buf
      line = index seq y
      (before, after) = P.splitAt x line
  in buf { lineSeq = update y (before ++ [char] ++ after) seq }


deleteCharFromBuffer :: Buffer -> Pos -> Buffer
deleteCharFromBuffer buf (Pos y x) =
  let seq = lineSeq buf
      line = index seq y
      (before, after) = P.splitAt x line
      newBefore = if not $ P.null before
                  then P.take (P.length before - 1) before
                  else before
  in buf { lineSeq = update y (newBefore ++ after) seq}

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
  in buf { lineSeq = (left >< (Seq.drop 1 right)) }

concatLine :: Buffer -> Int -> Buffer
concatLine buf idx =
  let seq = lineSeq buf
      (left, right) = Seq.splitAt (idx - 1) seq
      concatted = (Seq.index right 0) ++ (Seq.index right 1)
  in if idx <= 0 || (Seq.length right) < 2
     then buf
     else buf { lineSeq = (left |> concatted) >< (Seq.drop 2 right) }
