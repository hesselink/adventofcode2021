module Main where

import Control.Arrow ((***))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')

main :: IO ()
main = do
  f <- readFile "input/13"
  let (ps, fs) = parse f
      result = length . fold (head fs) $ ps
  print result
  let finalPs = foldl' (flip fold) ps fs
  putStrLn (ppr finalPs)

type Point = (Int, Int)

data Fold = Horizontal Int | Vertical Int
  deriving (Show, Eq)

parse :: String -> (Set Point, [Fold])
parse = ((Set.fromList . map parsePoint) *** (map parseFold . tail)) . break (== "") . lines
  where
    parsePoint str =
      let (hd, ',':tl) = break (== ',') str
      in (read hd, read tl)
    parseFold str =
      let (hd, '=':tl) = break (== '=') str
      in case last hd of
           'x' -> Vertical (read tl)
           'y' -> Horizontal (read tl)
           _ -> error $ "Unknown fold direction: " ++ hd

fold :: Fold -> Set Point -> Set Point
fold = Set.map . foldOne
  where
    foldOne :: Fold -> Point -> Point
    foldOne f (x, y) = case f of
      Horizontal l -> if y > l then (x, 2*l - y) else (x, y)
      Vertical l -> if x > l then (2*l - x, y) else (x, y)

ppr :: Set Point -> String
ppr ps =
  let l = Set.toList ps
      maxX = maximum . map fst $ l
      maxY = maximum . map snd $ l
  in unlines [ [ c | x <- [0..maxX], let c = if (x,y) `Set.member` ps then '#' else '.' ] | y <- [0..maxY] ]
