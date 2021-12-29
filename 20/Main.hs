module Main where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Arrow ((&&&))
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Maybe (fromMaybe, fromJust)

import Lib
import Lib.Point

main :: IO ()
main = do
  f <- readFile "input/20"
  let input = parse f
      output = enhance (enhance input)
      result = countLit (image output)
  print result
  let output2 = nTimes (50 :: Int) enhance input
      result2 = countLit (image output2)
  print result2

type Pixel = Char
type Image = Map Point Char

data State = State
  { lookups :: IntMap Pixel
  , image :: Image
  , defPixel :: Pixel
  } deriving (Show, Eq)

parse :: String -> State
parse str =
  let (i:"":ls) = lines str
  in State (parseLookups i) (parseImage ls) '.'

parseLookups :: String -> IntMap Pixel
parseLookups = IntMap.fromList . zip [0..]

parseImage :: [String] -> Image
parseImage = parseGrid id . unlines

enhance :: State -> State
enhance st@(State ls im def) =
  let ((minX, minY), (maxX, maxY)) = bounds im
      newImage = generateFrom st (minX - 1, minY - 1) (maxX + 1, maxY + 1)
      newDef = parseBinary . replicate 9 $ def
  in State ls newImage (fromJust . IntMap.lookup newDef $ ls)

bounds :: Image -> (Point, Point)
bounds im =
  let minX = fst . fst $ Map.findMin im
      maxX = fst . fst $ Map.findMax im
      ys = map snd . Map.keys $ im
      minY = minimum ys
      maxY = maximum ys
  in ((minX, minY), (maxX, maxY))

generateFrom :: State -> Point -> Point -> Image
generateFrom st tl br =
  Map.fromList . map (id &&& generatePoint st) $ pointsInBounds tl br

pointsInBounds :: Point -> Point -> [Point]
pointsInBounds (minX, minY) (maxX, maxY) =
  [ (x, y) | x <- [minX .. maxX], y <- [minY .. maxY] ]

generatePoint :: State -> Point -> Pixel
generatePoint (State ls im def) p =
  let ns = sortBy (comparing snd <> comparing fst) . (p:) . neighbours $ p
      cs = map (getPixel def im) ns
      n = parseBinary cs
  in fromJust . IntMap.lookup n $ ls

parseBinary :: String -> Int
parseBinary = readBinUnsafe . map zeroOne
  where
    zeroOne '.' = '0'
    zeroOne '#' = '1'
    zeroOne c = error $ "zeroOne: " ++ [c]

ppr :: Image -> String
ppr im =
  let (tl, br) = bounds im
      ps = pointsInBounds tl br
  in unlines . map (map (getPixel '?' im)) . groupBy ((==) `on` snd) . sortBy (comparing snd <> comparing fst) $ ps

getPixel :: Pixel -> Image -> Point -> Pixel
getPixel def im = fromMaybe def . flip Map.lookup im

countLit :: Image -> Int
countLit = length . filter (== '#') . Map.elems
