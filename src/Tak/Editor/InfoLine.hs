module Tak.Editor.InfoLine where

import Tak.Types
import Tak.Display
import Tak.Buffer


instance Editor InfoLineEditor where
  render editor height width = do
    invertText
    renderBuffer Crop (infoBuffer editor) Nothing height width

setInfoLineContent infoLineEditor str =
  infoLineEditor { infoBuffer = strToBuffer str }

