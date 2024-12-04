module Main (main) where

import Lib

main :: IO ()
main = do
  input <- readFile "input.txt"

  print (solve input)
