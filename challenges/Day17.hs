{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Char (digitToInt)
import Advent (challenge)
import Data.Maybe (fromJust)

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

dejkstra :: (Node -> [Node]) -> (Node -> Node -> Int) -> (Node -> Bool) -> [Node] -> Maybe Int
dejkstra stepFn costFn isDone startNodes = search (PQ.fromList $ zip [0..] startNodes) S.empty (M.fromList $ zip startNodes [0..])
  where
    search queue seen score = case PQ.minViewWithKey queue of
      Nothing -> Nothing
      Just ((cost, node), rest) ->
        if isDone node then Just cost
        else if S.member node seen then search rest seen score
        else search rest' seen' score'
          where
            seen' = S.insert node seen
            ok c n = not (S.member n seen') && (not (M.member n score) || c < (fromJust . M.lookup n $ score))
            neighbors = [(c, n) | n <- stepFn node
                                , let c = costFn node n
                                , ok cost n
                                ]
            rest' = foldr (\(c, n) q -> PQ.insert c n q) rest neighbors
            score' = foldr (\(c, n) s -> M.insert n c s) score neighbors

astarSearch :: (Node -> [Node]) -> (Node -> Node -> Int) -> (Node -> Bool) -> Node -> Maybe Int
astarSearch nextNodeFn costFn isGoalNode startNode =
  astar (PQ.singleton 0 startNode) S.empty (M.singleton startNode 0)
  where
    astar pq seen gscore = case PQ.minViewWithKey pq of
      Nothing -> Nothing
      Just ((cost, node), rest) ->
        if isGoalNode node then Just cost
        else if S.member node seen then astar rest seen gscore
        else astar pq'' seen' gscore'
          where
            seen'         = S.insert node seen
            ok c n = no
            successors    =
              filter (\(s, g) -> not (S.member s seen') &&
                        (not (s `M.member` gscore) || g < (fromJust . M.lookup s $ gscore)))
              $ [(node, c) | n <- nextNodeFn node, let c = costFn node n, not (S.member node seen')]
            pq''    = foldl (\q (s, g) -> PQ.insert g s q) rest successors
            gscore' = foldl (\m (s, g) -> M.insert s g m) gscore successors
    successorsAndCosts node gcost = map (\s -> (s, gcost + costFn node s)) . nextNodeFn $ node

solve :: (Int -> Bool) -> (Int -> Bool) -> Challenge -> Int
solve canTurn canMoveStraight (rs, cs, grid)  = fromJust $ dj
  where
    stepFn = (step canTurn canMoveStraight grid)
    edgeFn = (edge grid)
    doneFn = (\(s, (r, c), _) -> r == rs && c == cs && canTurn s)
    dj = astarSearch stepFn edgeFn doneFn (0, (1, 1), (1, 0))

part1 :: Challenge -> String
part1 = show . solve (const True) (< 3)

part2 :: Challenge -> String
part2 = show . solve (>= 4) (< 10)

main :: IO ()
main = challenge "17" parse part1 part2