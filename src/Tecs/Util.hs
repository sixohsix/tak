module Tecs.Util where

import Tecs.Types

clamp :: Int -> Int -> Int -> Int
clamp low high = max low . min high
