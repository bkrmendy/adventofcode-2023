{-# LANGUAGE Strict #-}
module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Advent (challenge)
import Utils ()

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
                            , nextPos <- [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
                            , let cell = grid M.! cc nextPos
                            , cell /= '#'
                            ]
  where
    cc (r, c) = (r `mod` rs, c `mod` cs)

go :: Int -> Grid -> Roots -> Roots
go 0 _ roots = roots
go n grid roots = go (n - 1) grid (step grid roots)

start :: Grid -> (Int, Int)
start (_, _, g) = head [s | (s, 'S') <- M.toList g]

reach :: Int -> Grid -> Int
reach steps grid = S.size $ go steps grid (S.singleton (start grid))

part1 :: Challenge -> String
part1 = show . reach 64

part2 :: Challenge -> String
part2 grid = show $ f (steps `quot` cs)
  where steps = 26501365
        (_, cs, _) = grid
        remainder = steps `mod` cs
        -- | TIL
        t0 = reach remainder grid
        t1 = reach (remainder + cs) grid
        t2 = reach (remainder + cs + cs) grid
        
        -- | TIL even more
        d01 = t1 - t0
        d12 = t2 - t1
        d01_12 = d12 - d01
        
        -- | MAX TIL
        f x = t0 + (t1-t0) * x + x * (x-1) `quot` 2 * d01_12
        
main :: IO ()
main = challenge "21" parse part1 part2