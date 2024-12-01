solve :: String -> String
solve content = answer
  where
    answer = "Answer goes here"

main = do
  input <- readFile "input.txt"

  print (solve input)
