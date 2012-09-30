module Tak.Editor.Replace where

import System.IO (hSetBinaryMode, hGetContents, hPutStr)
import System.Process (runInteractiveCommand)
import Control.Concurrent (forkIO)
import Control.Lens

import Tak.Types
import Tak.Buffer
import Tak.Buffer.LineSeq
import Tak.Editor.Selection
import Tak.Range


updateActiveBuffer :: (Buffer -> IO Buffer) -> GlobalState -> IO GlobalState
updateActiveBuffer f gst =
  traverseOf editor (\ed -> (f $ buffer ed) >>= (\newBuf -> return $ ed { buffer = newBuf })) gst


processCmd :: String -> String -> IO String
processCmd command inputStr = do
  (inp, out, err, pid) <- runInteractiveCommand command
  hSetBinaryMode inp False
  hSetBinaryMode out False
  forkIO (hPutStr inp inputStr)
  hGetContents out


processLineSeqIO :: (String -> IO String) -> LineSeq -> IO LineSeq
processLineSeqIO f lSeq = (f $ lineSeqToStr lSeq) >>= (return . strToLineSeq)


replaceRegion :: (LineSeq -> IO LineSeq) -> Buffer -> (Pos, Pos) -> IO Buffer
replaceRegion f buf r@(rStartPos, _) = do
  let (ls, cutBuf) = cutSelection buf r
  replacementLs <- f ls
  return $ insertLineSeqIntoBuffer cutBuf rStartPos replacementLs


repRegionWithShellCmd :: String -> Buffer -> (Pos, Pos) -> IO Buffer
repRegionWithShellCmd cmd = replaceRegion (processLineSeqIO $ processCmd cmd)


replaceRegionWithShellCmd :: String -> GlobalState -> IO GlobalState
replaceRegionWithShellCmd cmd gst =
  case currentSelection (view editor gst) of
    Nothing -> return $ gst
    Just region -> updateActiveBuffer (\buf -> repRegionWithShellCmd cmd buf $ asTuple region) gst

