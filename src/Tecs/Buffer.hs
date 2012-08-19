{-# LANGUAGE NoImplicitPrelude #-}

module Tecs.Buffer where

import Prelude as P
import Tecs.Types

bufferFromStr :: String -> Buffer
bufferFromStr s = Buffer _lines where
  _lines :: [Line]
  _lines = do
    line <- P.lines s
    return (Line line)
