{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Algorithm.Search (dijkstra)
import Data.Char (digitToInt)
import Advent (challenge)
import Utils ()
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)

type Grid = M.HashMap (Int, Int) Int
type Challenge = (Int, Int, Grid)

parse :: String -> Challenge
parse input = (length ls, length (head ls), ) $ M.fromList $ do
  (iRow, row) <- zip [1..] ls
  (iCol, cell) <- zip [1..] row
  pure ((iRow, iCol), digitToInt cell)
  where ls = lines input

turnLeft, turnRight :: (Int, Int) -> (Int, Int)
turnLeft (dr, dc) = (-dc, dr)
turnRight (dr, dc) = (dc, -dr)

type Node = (Int, (Int, Int), (Int, Int))

step :: (Int -> Bool) -> (Int -> Bool) -> Grid -> Node -> [Node]
step canTurn canMoveStraight grid (straight, (r, c), dir) = next (straightMoves ++ turnMoves)
  where
      straightMoves = if canMoveStraight straight then [(dir, straight + 1)] else []
      turnMoves = if canTurn straight then [(turnLeft dir, 1), (turnRight dir, 1)] else []
      next dirs = [(s, pos, (dr, dc))
                  | ((dr, dc), s) <- dirs
                  , let pos = (r + dr, c + dc)
                  , M.member pos grid
                  ]

edge :: Grid -> Node -> Node -> Int
edge grid _ (_, (r, c), _) = grid M.! (r, c)

solve :: (Int -> Bool) -> (Int -> Bool) -> Challenge -> Int
solve canTurn canMoveStraight (rs, cs, grid)  = fst $ fromJust $ dj
  where dj = dijkstra (step canTurn canMoveStraight grid) (edge grid) (\(s, (r, c), _) -> r == rs && c == cs && canTurn s) (0, (1, 1), (1, 0))

part1 :: Challenge -> String
part1 = show . solve (const True) (< 3)

part2 :: Challenge -> String
part2 = show . solve (>= 4) (< 10)

main :: IO ()
main = challenge "17" parse part1 part2