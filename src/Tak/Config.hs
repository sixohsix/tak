module Tak.Config where

import System.Directory (canonicalizePath, createDirectoryIfMissing, getHomeDirectory)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Control.Monad (guard)

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


loadCursorPositions = do
  configPath <- getTakConfigDir
  let posFilePath = configPath ++ "/cursor_positions"
  loadCursorPositionsInFile posFilePath


loadCursorPositionsInFile :: String -> IO [(FilePath, Pos)]
loadCursorPositionsInFile file = do
  pfContents <- readFile file
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
  configPath <- getTaxConfigDir
  let posFilePath = configPath ++ "/cursor_positions"
  writeFile posFilePath $ unparsePosLines posList

updatePosFile :: FilePath -> Pos -> IO ()
updatePosFile fn pos = do
  posList <- loadCursorPositions
  let newPosList = (fn, pos):(filter (\(fn', _) -> fn' /= fn))
  writeCursorPositions newPosList

