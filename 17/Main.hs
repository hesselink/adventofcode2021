module Main where

import qualified Text.Megaparsec as P
import Data.Void (Void)
import Control.Arrow ((&&&))

import Lib

main :: IO ()
main = do
  f <- readFile "input/17"
  let (p1, p2) = parse f
      maxVY = triangleNum (max (abs $ snd p1) (abs $ snd p2) - 1)
  print maxVY
  let minVY = min (snd p1) (snd p2)
      minVX = fst . head . dropWhile ((< min (fst p1) (fst p2)) . snd) . map (id &&& triangleNum) $ [1..]
      maxVX = max (fst p1) (fst p2)
      vs = [ (vx, vy) | vy <- [minVY .. maxVY], vx <- [minVX .. maxVX] ]
      goodVs = filter (intersects (p1, p2)) vs
  print (length goodVs)

type Point = (Int, Int)
type Parser = P.Parsec Void String

parse :: String -> (Point, Point)
parse = runParser parser

runParser :: Parser a -> String -> a
runParser p = either (error . P.errorBundlePretty) id . P.runParser p "input"

parser :: Parser (Point, Point)
parser
  = (\minX maxX minY maxY -> ((minX, minY), (maxX, maxY)))
  <$ P.chunk "target area: x="
  <*> parseNum
  <* P.chunk ".."
  <*> parseNum
  <* P.chunk ", y="
  <*> parseNum
  <* P.chunk ".."
  <*> parseNum

parseNum :: Parser Int
parseNum = read <$> P.takeWhileP (Just "digit") (`elem` "-0123456789")

intersects :: (Point, Point) -> Point -> Bool
intersects ((minX, minY), (maxX, maxY)) (vx, vy) =
  let steps = iterate step ((0,0), (vx,vy))
      step ((x', y'), (vx', vy')) = ((x' + vx', y' + vy'), (vx' - signum vx', vy' - 1))
      maybeIn = dropWhile (\(x', y') -> x' < minX && y' > maxY) . map fst $ steps
      ps = takeWhile (\(x', y') -> x' <= maxX && y' >= minY) $ maybeIn
      inBounds (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY
  in any inBounds ps
