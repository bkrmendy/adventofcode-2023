{-# LANGUAGE Strict #-}
module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Advent (challenge)
import Utils ()
import Debug.Trace (traceShow)

type Grid = (Int, Int, M.HashMap (Int, Int) Char)
type Roots = S.HashSet (Int, Int)
type Challenge = Grid

parse :: String -> Challenge
parse input = (rows, cols, grid)
  where 
    ls = lines input
    (rows, cols) = (length ls, length (head ls))
    grid = M.fromList $ do
      (iRow, row) <- zip [0..] ls
      (iCol, cell) <- zip [0..] row
      return ((iRow, iCol), cell)

step :: Grid -> Roots -> Roots
step (rs, cs, grid) roots = S.fromList
                            [ nextPos | (r, c) <- S.toList roots
                            , nextPos <- [cc (r + 1, c), cc (r - 1, c), cc (r, c + 1), cc (r, c - 1)]
                            , Just cell <- [M.lookup nextPos grid]
                            , cell == '.' || cell == 'S'
                            ]
  where
    cc (r, c) = (r `rem` rs, c `rem` cs)

go :: Int -> Grid -> Roots -> Roots
go 0 _ roots = roots
go n grid roots = traceShow n $ go (n - 1) grid (step grid roots)

start :: Grid -> (Int, Int)
start (_, _, g) = fst . head . filter (\(k, v) -> v == 'S') . M.toList $ g

part1 :: Challenge -> String
part1 grid = show . S.size $ go 64 grid (S.singleton (start grid))

part2 :: Challenge -> String
part2 grid = show . S.size $ go 26501365 grid (S.singleton (start grid))

main :: IO ()
main = challenge "21" parse part1 part2