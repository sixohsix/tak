module Tak.Editor.InfoLine where

import Tak.Types
import Tak.Display
import Tak.Buffer
import Tak.Util
import Tak.Editor.Cursor (insertPos)

import Control.Lens (set, over, view)
import Data.Monoid (mconcat)


instance Editor InfoLineEditor where
  render editor height width = do
    invertText
    renderBuffer Crop (infoBuffer editor) Nothing height width


setInfoLineContent infoLineEditor str =
  infoLineEditor { infoBuffer = strToBuffer str }


infoLineContent :: GlobalState -> Maybe String -> String
infoLineContent globalState extraMsg =
  let ed = view editor globalState
      modStr  = if isModified ed
                then "*"
                else " "
      fn      = fileName ed
      selSt   = selState ed
      Pos l r = insertPos ed
      posStr  = mconcat [(show (l + 1)), ":", (show r)]
      extra   = maybe "" (\m -> mconcat ["  [", m, "]"]) extraMsg
  in mconcat ["  ", modStr, "  ", fn, " ", posStr, extra]

