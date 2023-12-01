module Main where

import Advent (challenge)
import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf, tails)

type Challenge = [String]

parse :: String -> Challenge
parse = lines

p :: [Int] -> Int
p digits = 10 * (head digits) + last digits

part1 :: Challenge -> Int
part1 = sum . map (p . map digitToInt . filter isDigit)

numbers :: [(String, Int)]
numbers = [ ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6), ("7", 7), ("8", 8), ("9", 9)
         , ("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)
         ]

part2 :: Challenge -> Int
part2 = sum . map (p . calibrationValue)
  where
    calibrationValue :: String -> [Int]
    calibrationValue line = [n | value <- tails line, (literal, n) <- numbers, isPrefixOf literal value]

main :: IO ()
main = challenge "1" parse part1 part2