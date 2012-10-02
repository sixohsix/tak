module Tak.GlobalState.Clipboard where

import System.Directory (doesFileExist)
import qualified System.IO.Strict as Strict
import qualified Text.JSON as JSON
import Data.Foldable (toList)
import Data.Sequence (fromList, Seq)

import Tak.Types
import Tak.Config (getClipboardPath)

readClipboard :: IO [LineSeq]
readClipboard = do
  path <- getClipboardPath
  exists <- doesFileExist path
  if exists then do
     contents <- Strict.readFile path
     return $ case JSON.decode contents of
                JSON.Ok clipboard -> clipboard
                JSON.Error _ -> []
  else return []


writeClipboard :: [LineSeq] -> IO ()
writeClipboard clipboard = do
  path <- getClipboardPath
  let clipboardJson = JSON.encode clipboard
  writeFile path clipboardJson


instance JSON.JSON a => JSON.JSON (Seq a) where
  showJSON ls   = JSON.showJSON $ toList ls
  readJSON json = do
    list <- JSON.readJSON json
    return $ fromList list

