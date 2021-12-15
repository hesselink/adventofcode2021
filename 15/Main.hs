module Main where

import Data.Char (digitToInt, intToDigit)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.PSQueue as PSQueue
import Data.List (foldl', unfoldr)
import Data.PSQueue (Binding ((:->)))
import Data.Monoid (Sum(..))
import Control.Arrow ((***), first, second)

main :: IO ()
main = do
  f <- readFile "input/15"
  let grid = parse f
      path = shortestPath grid (0,0) (snd (bounds grid))
      result = totalRisk grid path
  print result
  let grid2 = multiplyGraph 5 grid
      path2 = shortestPath grid2 (0,0) (snd (bounds grid2))
      result2 = totalRisk grid2 path2
  print result2

data Graph = Graph
  { bounds :: (Point, Point)
  , weights :: Map Point Int
  } deriving (Show, Eq)

instance Semigroup Graph where
  Graph ((minX1, minY1), (maxX1, maxY1)) ws1 <> Graph ((minX2, minY2), (maxX2, maxY2)) ws2 = Graph ((min minX1 minX2, min minY1 minY2), (max maxX1 maxX2, max maxY1 maxY2)) (Map.union ws1 ws2)

instance Monoid Graph where
  mempty = Graph (maxBound, minBound) mempty

type Edge = Point -- target

type Point = (Int, Int)

type Path = [Point]

getEdges :: Point -> Graph -> [Edge]
getEdges (x, y) g = filter inBounds [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]
  where
    inBounds (x', y') =
      let ((minX, minY), (maxX, maxY)) = bounds g
      in x' >= minX && x' <= maxX && y' >= minY && y' <= maxY

getEdgeWeight :: Edge -> Graph -> Int
getEdgeWeight t = fromJust . Map.lookup t . weights

parse :: String -> Graph
parse = foldMap (uncurry parseLine) . zip [0..] . lines

parseLine :: Int -> String -> Graph
parseLine y str =
  Graph ((0, y), (length str - 1, y)) (Map.fromList $ zipWith (\x c -> ((x, y), digitToInt c)) [0..] str)

shortestPath :: Graph -> Point -> Point -> Path
shortestPath g start end = go (PSQueue.singleton start 0) Set.empty (Map.singleton start 0) Map.empty
  where
    go q explored ws parents =
      case PSQueue.minView q of
        Nothing -> error "No path"
        Just (cur :-> _, q') ->
          if cur == end
          then buildPath start end parents
          else
            let explored' = Set.insert cur explored
                neighbours = getEdges cur g
                notExplored = filter (not . (`Set.member` explored')) neighbours
                processNeighbour (q'', ws', ps) n =
                  let proposedWeight = fromJust (Map.lookup cur ws') + getEdgeWeight n g
                  in case Map.lookup n ws' of
                        Nothing -> (PSQueue.insert n proposedWeight q'', Map.insert n proposedWeight ws', Map.insert n cur ps)
                        Just w | proposedWeight < w -> (PSQueue.insert n proposedWeight q'', Map.insert n proposedWeight ws', Map.insert n cur ps)
                               | otherwise -> (q'', ws', ps)
                (newQ, newWs, newPs) = foldl' processNeighbour (q', ws, parents) notExplored
            in go newQ explored' newWs newPs

buildPath :: Point -> Point -> Map Point Point -> Path
buildPath start end parents = end : unfoldr step end
  where
    step cur =
      let prev = fromJust $ Map.lookup cur parents
      in if cur == start then Nothing else Just (prev, prev)

totalRisk :: Graph -> Path -> Int
totalRisk g = getSum . foldMap (Sum . fromJust . flip Map.lookup (weights g)) . init

multiplyGraph :: Int -> Graph -> Graph
multiplyGraph n g = mconcat $ concatMap (shifts n shiftDown) $ shifts n shiftRight g

shifts :: Int -> (Graph -> Graph) -> Graph -> [Graph]
shifts n shiftFun g = unfoldr step (n, g)
  where
    step (0, _) = Nothing
    step (n', g') =
      let shifted = shiftFun g'
      in Just (g', (n'-1, shifted))

shiftRight :: Graph -> Graph
shiftRight g@(Graph ((minX, _), (maxX, _)) _) =
  let width = maxX - minX + 1
  in shift (first (+width)) g

shiftDown :: Graph -> Graph
shiftDown g@(Graph ((_, minY), (_, maxY)) _) =
  let height = maxY - minY + 1
  in shift (second (+height)) g

shift :: (Point -> Point) -> Graph -> Graph
shift shiftPoint (Graph (tl, br) ws) =
  Graph
    (shiftPoint tl, shiftPoint br)
    (Map.fromList . map (shiftPoint *** inc) . Map.toList $ ws)

inc :: Int -> Int
inc 9 = 1
inc n = n + 1

ppr :: Graph -> String
ppr (Graph ((minX, minY), (maxX, maxY)) ws) = unlines $ map line [minY .. maxY]
  where
    line y = map (\x -> intToDigit . fromJust . Map.lookup (x, y) $ ws) [minX..maxX]
