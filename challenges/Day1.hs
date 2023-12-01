module Main where

import Advent (challenge)
import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf)

type Challenge = [String]

parse :: String -> Challenge
parse = lines

p :: [Int] -> Int
p digits = 10 * (head digits) + last digits

part1 :: Challenge -> Int
part1 = sum . map (p . map digitToInt . filter isDigit)

matchNumber :: String -> [Int]
matchNumber value = [n | (literal, n) <- numbers, isPrefixOf literal value]
  where
     numbers = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]

part2 :: Challenge -> Int
part2 = sum . map (p . calibrationValue)
  where
        calibrationValue :: String -> [Int]
        calibrationValue (d:rest) | isDigit d = (digitToInt d:calibrationValue rest)
        calibrationValue [] = []
        calibrationValue value = case matchNumber value of
          [number] -> (number:calibrationValue (tail value))
          _ -> calibrationValue (tail value)

main :: IO ()
main = challenge "1" parse part1 part2