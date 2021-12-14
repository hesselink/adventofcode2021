{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Char (digitToInt, intToDigit)
import Data.Sequence (Seq)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Data.Monoid (Sum (..), getSum)

import Lib
import Lib.Count

main :: IO ()
main = do
  f <- readFile "input/11"
  let grid = parseGrid f
      result = countAllFlashes 100 grid
  print result
  let result2 = findSync grid
  print result2

type Grid = Seq (Seq (Octo Int))
data Octo a = Flashed
            | Normal a
            deriving (Show, Eq, Ord, Functor)
type Point = (Int, Int)

parseGrid :: String -> Grid
parseGrid = Seq.fromList . map (Seq.fromList . map (Normal . digitToInt)) . lines

step :: Grid -> Grid
step = resetFlashed . handleFlashes . increaseEnergy

increaseEnergy :: Grid -> Grid
increaseEnergy = (fmap . fmap . fmap) (+1)

handleFlashes :: Grid -> Grid
handleFlashes g =
  let ps = findFlashing g
  in if null ps
     then g
     else handleFlashes . setFlashed ps . increaseNeighbours ps $ g

findFlashing :: Grid -> [Point]
findFlashing = Seq.foldMapWithIndex (\y l -> map (,y) . Seq.findIndicesL willFlash $ l)

neighbours :: Point -> [Point]
neighbours (x, y) = [(nx, ny) | nx <- [x - 1, x, x + 1], ny <- [y - 1, y, y + 1], not (nx == x && ny == y)]

increaseNeighbours :: [Point] -> Grid -> Grid
increaseNeighbours ps = onPoints (concatMap neighbours ps) (fmap (+1))

setFlashed :: [Point] -> Grid -> Grid
setFlashed ps = onPoints ps (const Flashed)

onPoints :: [Point] -> (Octo Int -> Octo Int) -> Grid -> Grid
onPoints ps f =
  let cs = count ps -- can improve here by following along with the index stepwise
  in Seq.mapWithIndex (\y -> Seq.mapWithIndex (\x -> nTimes (getCount (x, y) cs) f))

willFlash :: Octo Int -> Bool
willFlash = (> Normal 9)

resetFlashed :: Grid -> Grid
resetFlashed = fmap (fmap (\o -> if o == Flashed then Normal 0 else o))

ppr :: Grid -> String
ppr = unlines . toList . fmap (toList . fmap pprOcto)

pprOcto :: Octo Int -> Char
pprOcto (Normal n) = intToDigit n
pprOcto Flashed = 'X'

countAllFlashes :: Int -> Grid -> Int
countAllFlashes n g
  = (if (n == 0) then 0 else countAllFlashes (n - 1) (step g))
  + getSum (foldMap (foldMap isFlash) g)
  where
    isFlash (Normal 0) = Sum 1
    isFlash _ = Sum 0

findSync :: Grid -> Int
findSync = fromJust . findIndex isAllFlashes . iterate step

isAllFlashes :: Grid -> Bool
isAllFlashes = all (all (== Normal 0))
