module Main where

import Data.Map (Map)
import Data.Char (isLower)
import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as Map

main :: IO ()
main = do
  f <- readFile "input/12"
  let graph = parse f
      result = length . validPaths $ graph
  print result
  let result2 = length . validPaths2 $ graph
  print result2

type Cave = String
type Graph = Map Cave [Cave] -- connections from key to values

type Path = [Cave]

parse :: String -> Graph
parse = foldl' parseOne Map.empty . lines
  where
    parseOne m l =
      let (f, _:t) = break (== '-') l
      in Map.insertWith (++) t [f] . Map.insertWith (++) f [t] $ m

validPaths :: Graph -> [Path]
validPaths g = validPaths' [] g "start"

validPaths' :: Path -> Graph -> Cave -> [Path]
validPaths' visited _ "end" = ["end":visited]
validPaths' visited g cur =
  let cs = connections cur g
      validCs = filter (\c -> not (isSmall c && c `elem` visited)) cs
  in concatMap (validPaths' (cur:visited) g) validCs

connections :: Cave -> Graph -> [Cave]
connections c = fromMaybe [] . Map.lookup c

isSmall :: Cave -> Bool
isSmall = isLower . head

validPaths2 :: Graph -> [Path]
validPaths2 g = validPaths2' [] False g "start"

validPaths2' :: Path -> Bool -> Graph -> Cave -> [Path]
validPaths2' visited _ _ "end" = ["end":visited]
validPaths2' visited usedExtraVisit g cur =
  let cs = connections cur g
      validCs = mapMaybe isVisitAllowed cs
      isVisitAllowed c =
        if isSmall c
        then
          if c == "start"
          then Nothing
          else
            case length . filter (== c) $ visited of
              0 -> Just (c, usedExtraVisit)
              1 -> if not usedExtraVisit
                   then Just (c, True)
                   else Nothing
              _ -> Nothing
        else Just (c, usedExtraVisit)
  in concatMap (\(c, extra) -> validPaths2' (cur:visited) extra g c) validCs
