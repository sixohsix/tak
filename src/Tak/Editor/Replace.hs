module Tak.Editor.Replace where

import System.IO (hSetBinaryMode, forkIO, hGetContents, hPutStr)
import System.Process (runInteractiveProcess)

import Tak.Types
import Tak.Buffer
import Tak.Buffer.LineSeq
import Tak.Editor.Selection


updateActiveBuffer :: (Buffer -> IO Buffer) -> GlobalState -> IO GlobalState
updateActiveBuffer f gst =
  (f $ buffer $ editor gst) >>= (\newBuf -> gst { editor = (editor gst) { buffer = newBuf } })

processCmd :: String -> String -> IO String
processCmd command inputStr = do
  (inp, out, err, pid) <- runInteractiveProcess command
  hSetBinaryMode inp False
  hSetBinaryMode out False
  forkIO (hPutStr inp inputStr)
  hGetContents out


processLineSeqIO :: (String -> IO String) -> LineSeq -> IO LineSeq
processLineSeqIO f lSeq = (f $ lineSeqToStr lSeq) >>= (return . strToLineSeq)


replaceRegion :: (LineSeq -> IO LineSeq) -> Buffer -> (Pos, Pos) -> IO Buffer
replaceRegion f buf r@(rStartPos, _) = do
  let ls, cutBuf = cutSelection buf r
  replacementLs <- f ls
  return $ insertLineSeqIntoBuffer buf rStartPos ls


repRegionWithShellCmd :: String -> Buffer -> (Pos, Pos) -> IO Buffer
repRegionWithShellCmd cmd = replaceRegion (processLineSeqIO . (processCmd cmd))

replaceRegionWithShellCmd :: String -> GlobalState -> IO GlobalState
replaceRegionWithShellCmd cmd gst =
  updateActiveBuffer (
