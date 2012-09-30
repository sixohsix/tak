module Tak.GlobalState where

import Control.Lens

import Tak.Types

activeEditor = view editor

pasteable gst = case (view clipboard gst) of
  x:_       -> Just x
  otherwise -> Nothing

