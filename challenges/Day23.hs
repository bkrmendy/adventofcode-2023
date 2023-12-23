module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Advent (visual)

type Grid = M.HashMap (Int, Int) Char
type Challenge = Grid

parse :: String -> Challenge
parse input = M.fromList $ do
  (iRow, row) <- zip [0..] (lines input)
  (iCol, cell) <- zip [0..] row
  return ((iRow, iCol), cell)

type Step = Grid -> (Int, Int) -> [(Int, Int)]

step :: Step
step grid (r, c) = case grid M.! (r, c) of
  '.' -> [ (r', c')
         | (r', c') <- [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
         , Just cell <- [M.lookup (r', c') grid]
         , cell /= '#'
         ]
  '>' -> [(r, c + 1)]
  'v' -> [(r + 1, c)]

  -- the input doesn't actually have these
  '<' -> [(r, c - 1)]
  '^' -> [(r - 1, c)]

type End = (Int, Int) -> Bool
end :: Grid -> End
end grid = f
  where lastRow = maximum $ map fst (M.keys grid)
        f (r, _) = r == lastRow

smallGraphNodes :: End -> Grid -> (Int, Int) -> [((Int, Int), (Int, Int), Int)]
smallGraphNodes finished grid s = go s s 0 S.empty
  where
    go :: (Int, Int) -> (Int, Int) -> Int -> S.HashSet (Int, Int) -> [((Int, Int), (Int, Int), Int)]
    go begin coord dist seen
      | finished coord = [(begin, coord, dist)]
      | null next = []
      | length next == 1 = go begin (head next) (dist + 1) (S.insert coord seen)
      | otherwise = (begin, coord, dist):concatMap (\n -> go coord n 1 (S.insert coord seen)) next
      where next = [c | c <- step grid coord, not (S.member c seen)]

type Graph = M.HashMap (Int, Int) (S.HashSet ((Int, Int), Int))

walkSmallGraph :: End -> Graph -> (Int, Int) -> [Int]
walkSmallGraph finished graph begin = go begin S.empty 0
  where
      go :: (Int, Int) -> S.HashSet (Int, Int) -> Int -> [Int]
      go coord seen dist
        | finished coord = [dist]
        | null next = []
        | otherwise = concatMap (\(c, d) -> go c (S.insert coord seen) d) next
        where next = [(c, d + dist) | (c, d) <- S.toList $ graph M.! coord, not (S.member c seen)]

start :: Grid -> (Int, Int)
start grid = head $ [(0, c) | ((0, c), '.') <- M.toList grid]

solve :: (Graph -> Graph) -> Grid -> Int
solve transform grid = maximum $ walkSmallGraph finished graph begin
  where
   finished = end grid
   begin = start grid
   graph = transform $ M.fromListWith S.union $ do
     (from, to, dist) <- smallGraphNodes finished grid begin
     return (from, S.singleton (to, dist))

part1 :: Challenge -> String
part1 = show . solve id

undirected :: Graph -> Graph
undirected original = M.fromListWith S.union $ do
  (from, edges) <- M.toList original
  (to, dst) <- S.toList edges
  [(from, S.singleton (to, dst)), (to, S.singleton (from, dst))]

part2 :: Challenge -> String
part2 = show . solve undirected

main :: IO ()
main = visual "23" parse part1 part2