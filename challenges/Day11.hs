module Main where

import qualified Data.HashSet as S

import Advent (challenge)
import Utils (transpose)

type Challenge = (S.HashSet (Int, Int), S.HashSet Int, S.HashSet Int)

parse :: String -> Challenge
parse input = (galaxies, emptyRows, emptyCols)
  where
    ls = lines input
    galaxies = S.fromList [(r, c) | (r, row) <- zip [0..] ls, (c, cell) <- zip [0..] row, cell == '#' ]
    emptyRows = S.fromList [r | (r, row) <- zip [0..] ls, all (== '.') row]
    emptyCols = S.fromList [c | (c, col) <- zip [0..] (transpose ls), all (== '.') col]

distance :: Int -> (Int -> Bool) -> Int -> Int -> Int
distance expansion isEmpty a b = sum $ map space [from + 1..to] 
   where
    space i = if isEmpty i then expansion else 1
    (from, to) = if a <= b then (a, b) else (b, a)

allDistances :: Int -> Challenge -> Int
allDistances expansion (galaxies, emptyRows, emptyCols) = sum distances `div` 2
  where
    isEmptyRow = flip (S.member) emptyRows
    isEmptyCol = flip (S.member) emptyCols
    distances = do
      (r1, c1) <- S.toList galaxies
      (r2, c2) <- S.toList galaxies
      let dr = distance expansion isEmptyRow r1 r2
      let dc = distance expansion isEmptyCol c1 c2
      return $ dr + dc
      
part1 :: Challenge -> String
part1 = show . allDistances 2 

part2 :: Challenge -> String
part2 = show . allDistances 1000000

main :: IO ()
main = challenge "11" parse part1 part2