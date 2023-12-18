{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Char (digitToInt)
import Advent (challenge)
import Data.Maybe (maybe, fromJust)

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

dejkstra :: (Node -> [Node]) ->(Node -> Node -> Int) -> (Node -> Bool) -> [Node] -> Maybe Int
dejkstra nextNodeFn costFn isGoalNode startNodes =
  search (PQ.fromList $ zip [0..] startNodes)
         S.empty (M.fromList $ zip startNodes [0..])
  where
    search pq seen score = case PQ.minViewWithKey pq of
      Nothing -> Nothing
      Just ((cost, node), pq') ->
        if isGoalNode node then Just cost
        else if S.member node seen then search pq' seen score
        else search pq'' seen' score'
          where
            seen' = S.insert node seen
            successors = [(c, neighbor) | neighbor <- nextNodeFn node
                                        , let c = cost + costFn node neighbor
                                        -- ^ add the incremental cost to the cost so far
                                        , not (S.member neighbor seen')
                                        , maybe True (c <) (M.lookup neighbor score)
                                        -- ^ only add the nodes of the distance is unknown
                                        -- | or is shorter than the one we know of
                                        ]
            pq'' = foldl (\q (g, s) -> PQ.insert g s q) pq' successors
            score' = foldl (\m (g, s) -> M.insert s g m) score successors

solve :: (Int -> Bool) -> (Int -> Bool) -> Challenge -> Int
solve canTurn canMoveStraight (rs, cs, grid)  = fromJust $ dj
  where
    stepFn = (step canTurn canMoveStraight grid)
    edgeFn = (edge grid)
    doneFn = (\(s, (r, c), _) -> r == rs && c == cs && canTurn s)
    dj = dejkstra stepFn edgeFn doneFn [(0, (1, 1), (1, 0)), (0, (1, 1), (0, 1))]

part1 :: Challenge -> String
part1 = show . solve (const True) (< 3)

part2 :: Challenge -> String
part2 = show . solve (>= 4) (< 10)

main :: IO ()
main = challenge "17" parse part1 part2