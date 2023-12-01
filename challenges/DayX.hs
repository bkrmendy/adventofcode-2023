module Main where

import Advent (challenge)
import Utils ()

type Challenge = String

parse :: String -> Challenge
parse = id

part1 :: Challenge -> String
part1 = id

part2 :: Challenge -> String
part2 = id

main :: IO ()
main = challenge "00" parse part1 part2