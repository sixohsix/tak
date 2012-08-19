module Tecs.Text where

import Prelude
import Tecs.Types

wrapString :: Int -> String -> [String]
wrapString _ "" = [""]
wrapString width str = wrapString' width str
  where wrapString' _ "" = []
        wrapString' width str =
          let (left, right) = Prelude.splitAt width str
          in left:(wrapString' width right)


linesToFixedLengthStrs :: WrapMode -> Int -> [Line] -> [String]
linesToFixedLengthStrs wrapMode width lines =
  let lineToStrs (Line line) = case wrapMode of
        Crop      -> [take width line]
        otherwise -> wrapString width line
  in concat $ map lineToStrs lines
