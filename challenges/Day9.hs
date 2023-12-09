module Main where

import Advent (challenge)
import Utils (readInt)

type Challenge = [[Int]]

parse :: String -> Challenge
parse = map (map readInt . words) . lines

diffs :: [Int] -> [Int]
diffs [a, b] = [b - a]
diffs (a:b:rest) = (b - a:diffs (b:rest))

ends :: [Int] -> (Int, Int)
ends (x:xs) = go x xs
  where go f [l] = (f, l)
        go f (_:rest) = go f rest

build :: [Int] -> (Int, Int)
build xs | all (== 0) xs = (0, 0)
build xs = (f - nf, l + nl)
  where (f, l) = ends xs
        (nf, nl) = build $ diffs xs

part1 :: Challenge -> String
part1 = show . sum . map (snd . build)

part2 :: Challenge -> String
part2 = show . sum . map (fst . build)

main :: IO ()
main = challenge "9" parse part1 part2