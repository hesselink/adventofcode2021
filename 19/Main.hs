module Main where

import Data.List (sort, nub)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)

import qualified Data.Set as Set
import Data.Set (Set)

main :: IO ()
main = do
  f <- readFile "input/19"
  let st = parse f
      ps = findAllPoints st
  print (length ps)

data State = State
  { scanners :: [Scanner]
  } deriving (Eq, Show)

type Scanner = Set Point3

type Point3 = (Int, Int, Int)

parse :: String -> State
parse = State . map parseScanner . splitOn [""] . lines

parseScanner :: [String] -> Scanner
parseScanner = Set.fromList . map parsePoint3 . tail

parsePoint3 :: String -> Point3
parsePoint3 str =
  let [x,y,z] = splitOn "," str
  in (read x, read y, read z)

rotateX :: Point3 -> Point3
rotateX (x, y, z) = (x, -z, y)

rotateY :: Point3 -> Point3
rotateY (x, y, z) = (-z, y, x)

rotateZ :: Point3 -> Point3
rotateZ (x, y, z) = (-y, x, z)

allRotations :: Scanner -> [Scanner]
allRotations sc = map Set.fromList . nub . sort $ [ map (fx . fy . fz) (Set.toList sc) | fx <- iterations rotateX, fy <- iterations rotateY, fz <- iterations rotateZ  ]
  where
    iterations f = take 4 (iterate (f .) id)

translation :: Point3 -> Point3 -> Point3
translation (x1, y1, z1) (x2, y2, z2) = (x2 - x1, y2 - y1, z2 - z1)

-- translated points of second scanner to first scanner's frame of reference
match12 :: Scanner -> Scanner -> Maybe Scanner
match12 sc1 sc2 = listToMaybe
  [ sc2' | p1 <- Set.toList sc1
         , p2 <- Set.toList sc2
         , let t = translation p2 p1
         , let sc2' = Set.map (translate t) sc2
         , Set.size (Set.intersection sc1 sc2') >= 12
  ]

translate :: Point3 -> Point3 -> Point3
translate (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

-- translated points of second scanner to first scanner's frame of reference
matchAnyRotation :: Scanner -> Scanner -> Maybe Scanner
matchAnyRotation sc1 sc2 = listToMaybe
  [ sc2'' | sc2' <- allRotations sc2 , Just sc2'' <- [match12 sc1 sc2'] ]

findAllPoints :: State -> Scanner
findAllPoints (State []) = mempty
findAllPoints (State (sc0:scRest)) = go sc0 scRest
  where
    go ps [] = ps
    go ps (sc:scs) =
      case matchAnyRotation ps sc of
        Just ps' -> go (Set.union ps ps') scs
        Nothing -> go ps (scs ++ [sc])
