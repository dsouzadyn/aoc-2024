import Data.List

converListToIntList :: [String] -> [Int]
converListToIntList = map read

countOccurence :: Int -> [Int] -> Int
countOccurence a = length . filter (== a)

solve :: String -> Int
solve content = answer
  where
    firstList = converListToIntList (map (head . words) (lines content))
    secondList = converListToIntList (map (last . words) (lines content))
    answer = sum (zipWith (*) firstList (map (`countOccurence` secondList) firstList))

main = do
  input <- readFile "input.txt"

  print (solve input)
