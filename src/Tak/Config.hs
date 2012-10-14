module Tak.Config (getInitialPosition, updateInitialPosition, getClipboardPath) where

import System.Directory (canonicalizePath, createDirectoryIfMissing, getHomeDirectory, doesFileExist)
import qualified System.IO.Strict as Strict
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Monoid (mconcat)
import Control.Monad (guard, when)

import Tak.Types


mkdirp = createDirectoryIfMissing True


splitChr :: Char -> (String, [String]) -> Char -> (String, [String])
splitChr splitChar (s, strs) c =
  if c == splitChar
  then ("", strs ++ [s])
  else (s ++ [c], strs)


split :: Char -> String -> [String]
split c str = case foldl' (splitChr c) ("", []) str of
                ([],  strs) -> strs
                (str, strs) -> strs ++ [str]


getTakConfigDir :: IO FilePath
getTakConfigDir = do
  home <- getHomeDirectory
  let configDir = home ++ "/.tak"
  mkdirp configDir
  return $ configDir

getConfigFilePath :: FilePath -> IO FilePath
getConfigFilePath fn = do
  dir <- getTakConfigDir
  return (dir ++ "/" ++ fn)


getPosFilePath :: IO FilePath
getPosFilePath = getConfigFilePath "cursor_positions"


getClipboardPath :: IO FilePath
getClipboardPath = getConfigFilePath "clipboard.json"


loadCursorPositions = do
  configPath <- getTakConfigDir
  let posFilePath = configPath ++ "/cursor_positions"
  loadCursorPositionsInFile posFilePath


loadCursorPositionsInFile :: String -> IO [(FilePath, Pos)]
loadCursorPositionsInFile file = do
  exists <- doesFileExist file
  pfContents <- if exists
                then Strict.readFile file
                else return ""
  return $ parsePosFileContents pfContents


parsePosFileContents :: String -> [(FilePath, Pos)]
parsePosFileContents contents = do
  line <- lines contents
  parsed <- return $ parsePosLine line
  guard (parsed /= Nothing)
  return $ fromJust parsed


parsePosLine l =
  case split ' ' l of
    fn:line:row:_ ->
      case (reads line, reads row) of
        ((li, _):[], (ri, _):[]) -> Just (fn, Pos li ri)
        otherwise -> Nothing
    otherwise -> Nothing

unparsePosLine (fn, Pos l r) = mconcat [fn, " ", show l, " ", show r]
unparsePosLines = unlines . map unparsePosLine

writeCursorPositions posList = do
  configPath <- getTakConfigDir
  let posFilePath = configPath ++ "/cursor_positions"
  writeFile posFilePath $ unparsePosLines posList

updateInitialPosition :: FilePath -> Pos -> IO ()
updateInitialPosition fp pos = do
  exists <- doesFileExist fp
  when exists $ do
    absFp <- canonicalizePath fp
    posList <- loadCursorPositions
    let newPosList = (absFp, pos):(filter (\(fp', _) -> fp' /= absFp) posList)
    writeCursorPositions newPosList

getInitialPositionOfRealFile fp = do
  absFp <- canonicalizePath fp
  posns <- loadCursorPositions
  return $ case filter (\(fp, _) -> fp == absFp) posns of
             ((_, pos):_) -> pos
             otherwise    -> Pos 0 0

getInitialPosition :: FilePath -> IO Pos
getInitialPosition fp = do
  exists <- doesFileExist fp
  pos <- if exists
         then getInitialPositionOfRealFile fp
         else return $ Pos 0 0
  return pos
