import Data.List

converListToIntList :: [String] -> [Int]
converListToIntList = map read

solve :: String -> Int
solve content = answer
  where
    firstList = converListToIntList (map (head . words) (lines content))
    secondList = converListToIntList (map (last . words) (lines content))
    answer = sum (map abs (zipWith (-) (sort firstList) (sort secondList)))

main = do
  input <- readFile "input.txt"

  print (solve input)
