import Data.List

converListToIntList :: [String] -> [Int]
converListToIntList = map read

validDifference :: Int -> Int -> Bool
validDifference a b = abs (a - b) <= 3

isIncreasing :: [Int] -> Bool
isIncreasing xs = and $ zipWith (\x y -> (<) x y && validDifference x y) xs (tail xs)

isDecreaseing :: [Int] -> Bool
isDecreaseing xs = and $ zipWith (\x y -> (>) x y && validDifference x y) xs (tail xs)

isSafe :: [Int] -> Bool
isSafe xs = isIncreasing xs || isDecreaseing xs

solve :: String -> Int
solve content = answer
  where
    answer = sum (map (fromEnum . isSafe . converListToIntList . words) (lines content))

main = do
  input <- readFile "input.txt"

  print (solve input)
