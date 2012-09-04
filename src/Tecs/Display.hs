module Tecs.Display where

import qualified UI.HSCurses.CursesHelper as CH
import qualified UI.HSCurses.Curses as C
import Prelude
import Control.Monad
import Control.Monad.Writer
import Data.Char (chr, ord)
import Data.Bits ( (.|.) )
import System.Locale.SetLocale
import qualified Data.ByteString as B
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import Foreign.C.Types (CInt)

import Tecs.Types

import Debug.Trace (trace)


withCurses :: IO () -> IO ()
withCurses f = do
  setLocale LC_ALL Nothing
  CH.start
  C.echo False
  C.keypad C.stdScr True
  C.nl True
  (y, x) <- getScreenSize
  C.move (y-1) (x-1)
  f
  C.endWin
  CH.end

clearScreen = C.wclear C.stdScr

getScreenSize :: IO (Int, Int)
getScreenSize = C.scrSize

clamp :: Int -> Int -> Int -> Int
clamp low high = max low . min high

refresh = C.refresh

cursesKeyToEvt :: C.Key -> Event
cursesKeyToEvt (C.KeyChar '\n')   = KeyEvent KeyEnter
cursesKeyToEvt (C.KeyChar '\DEL') = KeyEvent KeyDel
cursesKeyToEvt (C.KeyChar c)
  | (ord c) >= 32 = KeyEvent $ KeyChar c
  | otherwise     = KeyEvent $ KeyCtrlChar (chr ((ord c) .|. (2 ^ 6)))
cursesKeyToEvt C.KeyUp            = KeyEvent KeyUp
cursesKeyToEvt C.KeyDown          = KeyEvent KeyDown
cursesKeyToEvt C.KeyLeft          = KeyEvent KeyLeft
cursesKeyToEvt C.KeyRight         = KeyEvent KeyRight
cursesKeyToEvt C.KeyEnter         = KeyEvent KeyEnter
cursesKeyToEvt C.KeyNPage         = KeyEvent KeyPageDown
cursesKeyToEvt C.KeyPPage         = KeyEvent KeyPageUp
cursesKeyToEvt _                  = NoEvent


waitEvent :: IO (Event)
waitEvent = let
  isValidFirstKey key =
    key <= 255

  isValidNextKey key =
    key >= 128 && key <= 191

  nMoreBytes firstKey
    | firstKey <= 127                     = 0
    | firstKey >= 194 && firstKey <= 223  = 1
    | firstKey >= 224 && firstKey <= 239  = 2
    | firstKey >= 240 && firstKey <= 244  = 3
    | otherwise                           = 0

  cIntToBs :: [CInt] -> B.ByteString
  cIntToBs l = B.pack $ map (fromIntegral . toInteger) l

  getNextKey = C.getch

  decodeKey :: CInt -> IO (C.Key)
  decodeKey firstKey =
    if isValidFirstKey firstKey
    then decodeAfterNMore (nMoreBytes firstKey) [firstKey]
    else return $ C.decodeKey firstKey

  decodeAfterNMore :: Int -> [CInt] -> IO (C.Key)
  decodeAfterNMore nBytes ints =
    if nBytes > 0
    then do
      key <- getNextKey
      if isValidNextKey key
        then decodeAfterNMore (nBytes - 1) (ints ++ [key])
        else decodeKey key
    else decodeInts ints

  decodeInts :: [CInt] -> IO (C.Key)
  decodeInts ints =
    let doYourBest = return $ C.decodeKey (ints !! 0)
    in case (DTE.decodeUtf8' . cIntToBs) ints of
      Right bs  -> case DT.unpack bs of
        c:_         -> return $ C.KeyChar c
        otherwise   -> doYourBest
      otherwise -> doYourBest

  in do
    firstKeyCInt <- getNextKey
    utf8DecodedKey <- decodeKey firstKeyCInt
    return $ cursesKeyToEvt utf8DecodedKey

printStr :: Pos -> String -> RenderW ()
printStr p s = RenderW ((), [PrintStr p s])
setCursor :: Pos -> RenderW ()
setCursor p = RenderW ((), [SetCursor p])

drawToScreen :: Box -> RenderAction -> IO ()
drawToScreen (Box top left height width) command =
  let clampLine = clamp top  (top  + height - 1)
      clampRow  = clamp left (left + width - 1)
  in
    case command of
      SetCursor (Pos line row) -> C.move (clampLine (top + line))
                                         (clampRow  (left + row))
      PrintStr (Pos line row) str -> do
        let realLine  = line + top
            realRow   = row + left
            realWidth = width - realRow - 1
            realStr   = take realWidth (str ++ (repeat ' '))
        C.move (clampLine realLine) (clampRow realRow)
        C.wAddStr C.stdScr realStr
        return ()

