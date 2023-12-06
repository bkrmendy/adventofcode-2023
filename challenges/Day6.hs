module Main where

import Data.List.Split
import Advent (challenge)
import Utils (readInt)

type Challenge = ([String], [String])

parse :: String -> Challenge
parse input = 
    let
      [times, distances] = lines input
      mk = drop 1 . filter (not . null)
      ts = mk $ splitOn " " times
      ds = mk $  splitOn " " distances
    in (ts, ds)

race :: Int -> Int -> [Int]
race time distance = [d | speed <- [0..time], let t = time - speed, let d = t * speed, d > distance]

part1 :: Challenge -> String
part1 (ts, ds) = show $ product $ [length ways | (time, distance) <- rs, let ways = race time distance]
  where rs = zip (readInt <$> ts) (readInt <$> ds)

part2 :: Challenge -> String
part2 (ts, ds) = show . length $ race t d
  where t = readInt $ concat ts
        d = readInt $ concat ds 

main :: IO ()
main = challenge "6" parse part1 part2