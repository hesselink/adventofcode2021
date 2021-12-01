module Main where

main :: IO ()
main = do
  f <- readFile "input/1"
  let depths = parse f
      result = solve depths
  print result
  let result2 = solve2 depths
  print result2

parse :: String -> [Int]
parse = map read . lines

solve :: [Int] -> Int
solve = countIncreases

countIncreases :: [Int] -> Int
countIncreases xs = length . filter (uncurry (>)) . zip (tail xs) $ xs

solve2 :: [Int] -> Int
solve2 depths = countIncreases $ slidingWindows depths

slidingWindows :: [Int] -> [Int]
slidingWindows xs = zipWith3 (\x y z -> x + y + z) xs (tail xs) (tail . tail $ xs)
