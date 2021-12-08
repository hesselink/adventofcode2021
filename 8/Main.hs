module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List ((\\), sort, group, elemIndex)

main :: IO ()
main = do
  f <- readFile "input/8"
  let digits = parse f
      result = count1478 digits
  print result
  let inputs = parse2 f
      result2 = sum $ map solve inputs
  print result2

parse :: String -> [String]
parse = concatMap (words . drop 2 . snd . break (== '|')) . lines

count1478 :: [String] -> Int
count1478 = length . filter ((`elem` [2,3,4,7]) . length)

type Pattern = [Segment]
type Segment = Char

data Input = Input
  { patterns :: [Pattern]
  , value :: [Pattern]
  } deriving (Show, Eq)

parse2 :: String -> [Input]
parse2 = map parseOne . lines
  where
    parseOne str =
      let (ps, v) = break (== '|') str
      in Input (words ps) (words (drop 2 v))

solve :: Input -> Int
solve i = valueFromMappings (computeMappings (patterns i)) (value i)

computeMappings :: [Pattern] -> Map Segment Segment
computeMappings ps =
  let fromA = computeA ps
      (fromE, fromG) = computeEG fromA ps
      (fromB, fromD) = computeBD [fromA, fromE, fromG] ps
      (fromC, fromF) = computeCF [fromA, fromB, fromD, fromE, fromG] ps
  in Map.fromList $ zip [fromA, fromB, fromC, fromD, fromE, fromF, fromG] "abcdefg"

computeA :: [Pattern] -> Segment
computeA ps =
  let [one] = filter ((== 2) . length) ps
      [seven] = filter ((== 3) . length) ps
      [a] = seven \\ one
  in a

computeEG :: Segment -> [Pattern] -> (Segment, Segment)
computeEG a ps =
  let psNo1478 = filter (not . (`elem` [2,3,4,7]) . length) ps
      segmentsGrouped = group . sort . filter (/= a) . concat $ psNo1478
      e = head . head . filter ((== 3) . length) $ segmentsGrouped
      g = head . head . filter ((== 6) . length) $ segmentsGrouped
  in (e, g)

computeBD :: [Segment] -> [Pattern] -> (Segment, Segment)
computeBD used ps =
  let psLen5 = filter ((== 5) . length) ps
      segmentsGrouped = group . sort . filter (not . (`elem` used)) . concat $ psLen5
      b = head . head . filter ((== 1) . length) $ segmentsGrouped
      d = head . head . filter ((== 3) . length) $ segmentsGrouped
  in (b, d)

computeCF :: [Segment] -> [Pattern] -> (Segment, Segment)
computeCF used ps =
  let psLen6 = filter ((== 6) . length) ps
      segmentsGrouped = group . sort . filter (not . (`elem` used)) . concat $ psLen6
      c = head . head . filter ((== 2) . length) $ segmentsGrouped
      f = head . head . filter ((== 3) . length) $ segmentsGrouped
  in (c, f)

valueFromMappings :: Map Segment Segment -> [Pattern] -> Int
valueFromMappings m = convert . map valueFromMappings1
  where
    valueFromMappings1 = fromJust . flip elemIndex digits . sort . map (fromJust . flip Map.lookup m)
    digits = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

convert :: [Int] -> Int
convert = sum . zipWith (*) (iterate (*10) 1) . reverse
