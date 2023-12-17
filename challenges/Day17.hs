{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.PQueue.Prio.Min as PQ
import Data.Char (digitToInt)
import Advent (challenge)
import Utils ()
import Debug.Trace (traceShow)

type Grid = M.HashMap (Int, Int) Int
type Challenge = (Int, Int, Grid)

parse :: String -> Challenge
parse input = (length ls, length (head ls), ) $ M.fromList $ do
  (iRow, row) <- zip [0..] ls
  (iCol, cell) <- zip [0..] row
  pure ((iRow, iCol), digitToInt cell)
  where ls = lines input

turnLeft, turnRight :: (Int, Int) -> (Int, Int)
turnLeft (dr, dc) = (-dc, dr)
turnRight (dr, dc) = (dc, -dr)

type Node = (Int, (Int, Int), (Int, Int))

step :: Grid -> (Int, Node) -> [(Int, Node)]
step grid (dist, (straight, (r, c), dir)) =
  if straight < 3
  then next [(dir, straight + 1), (turnLeft dir, 1), (turnRight dir, 1)]
  else next [(turnLeft dir, 1), (turnRight dir, 1)]
  where
      next dirs = [(dist + cell, (s, pos, (dr, dc)))
                  | ((dr, dc), s) <- dirs
                  , let pos = (r + dr, c + dc)
                  , Just cell <- [M.lookup pos grid]
                  ]

type Distances = M.HashMap (Int, Int) Int

update :: [(Int, Node)] -> PQ.MinPQueue Int Node -> PQ.MinPQueue Int Node
update xs queue = foldr (\(k, v) q -> PQ.insert k v q) queue xs

walk
  :: (Int, Int, Grid)
  -> PQ.MinPQueue Int Node
  -> S.HashSet (Int, Int)
  -> Int
walk (rows, cols, grid) queue seen = case PQ.minViewWithKey queue of
  Nothing -> error "pq empty"
  Just ((dist, (straight, (r, c), dir)), rest) ->
      if r == rows - 1 && c == cols - 1
      then dist
      else walk (rows, cols, grid) (update next rest) (S.insert (r, c) seen)
    where
      next = filter (\(_, (_, p, _)) -> not $ S.member p seen) $ step grid (dist, (straight, (r, c), dir))

part1 :: Challenge -> String
part1 (rs, cs, grid) = show $ run
  where
    run = walk (rs, cs, grid) (PQ.fromList [(0, (0, (0, 0), (1, 0))), (0, (0, (0, 0), (0, 1)))]) S.empty

part2 :: Challenge -> String
part2 = const "-"

main :: IO ()
main = challenge "17" parse part1 part2