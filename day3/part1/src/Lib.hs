
module Lib
  ( solve,
  )
where

import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

convertToInt :: String -> Int
convertToInt s = read s :: Int

toIntPair :: [String] -> (Int, Int)
toIntPair xs = (convertToInt (head xs), convertToInt (last xs))

parseToMulFuncs :: String -> [String]
parseToMulFuncs content = getAllTextMatches (content =~ "mul\\([0-9]+\\,[0-9]+\\)") :: [String]

parseToStringList :: String -> [String]
parseToStringList content = getAllTextMatches (content =~ "[0-9]+") :: [String]


solve :: String -> Int
solve content = answer
  where
    answer = sum (map (uncurry (*) . toIntPair . parseToStringList) (parseToMulFuncs content))
