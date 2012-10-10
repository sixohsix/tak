module Tak.Text where

import Prelude
import qualified Data.Text as Text
import Tak.Types

wrapLine :: Int -> Line -> [Line]
wrapLine width str
  | str == Text.empty = [Text.empty]
  | otherwise = wrapString' width str
      where wrapString' width str
              | str == Text.empty = []
              | otherwise =
                  let (left, right) = Text.splitAt width str
                  in left:(wrapString' width right)


linesToFixedLengthStrs :: WrapMode -> Int -> [Line] -> [String]
linesToFixedLengthStrs wrapMode width lines =
  let lineToStrs line = case wrapMode of
        Crop      -> [Text.take width line]
        otherwise -> wrapLine width line
  in map Text.unpack $ concat $ map lineToStrs lines

