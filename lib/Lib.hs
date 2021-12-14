module Lib where

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ = id
nTimes n f = f . nTimes (n - 1) f
