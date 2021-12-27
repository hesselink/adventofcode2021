module Lib where

import Data.Char (digitToInt)
import Numeric (readInt)

nTimes :: Integral n => n -> (a -> a) -> a -> a
nTimes 0 _ = id
nTimes n f = f . nTimes (n - 1) f

readBin :: (Num a, Eq a) => ReadS a
readBin = readInt 2 (`elem` "01") digitToInt

readBinUnsafe :: (Num a, Eq a) => String -> a
readBinUnsafe = fst . head . readBin
