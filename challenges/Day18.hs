module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL(EmptyL, (:<)), (><))
import Data.List (foldl')
import Advent (challenge, visual)
import Utils (readInt)
import Numeric (readHex)
import Debug.Trace (traceShow)

type Grid = M.HashMap (Int, Int) String
type Pick = ((String, Int), (String, Int)) -> (String, Int)
type Challenge = [((String, Int), (String, Int))]

parseP2Instr :: String -> (String, Int)
parseP2Instr e = case e of
  [_, '#', a, b, c, d, e, dn, _] -> (dir dn, rh [a,b,c,d,e])
  e -> error e
  where
    rh :: String -> Int
    rh i = fst $ head $ (readHex i)
    dir :: Char -> String
    dir '0' = "R"
    dir '1' = "D"
    dir '2' = "L"
    dir '3' = "U"

parse :: String -> Challenge
parse = map p . lines
  where
    p1 d l = (d, readInt l)
    p line = (\[d, l, c] -> (p1 d l, parseP2Instr c)) . words $ line

move :: (Int, Int) -> String -> (Int, Int)
move (r, c) "R" = (r, c + 1)
move (r, c) "L" = (r, c - 1)
move (r, c) "U" = (r - 1, c)
move (r, c) "D" = (r + 1, c)

dig :: [(String, Int)] -> Grid
dig = fst . foldl' go (M.empty, (0, 0))
  where
    go :: (Grid, (Int, Int)) -> (String, Int) -> (Grid, (Int, Int))
    go (grid, pos) (dir, n) = foldl' (\(m, p) _ -> (M.insert p "#" m, move p dir)) (grid, pos) [1..n]

start :: Grid -> (Int, Int)
start grid = (rsMin + 1, csMin + 1)
  where
    rsMin = minimum $ [r | (r, _) <- M.keys grid]
    csMin = minimum $ [c | (r, c) <- M.keys grid, r == rsMin]

fill :: Grid -> Int
fill grid = go (Seq.singleton (mr, mc)) S.empty 0
  where
    (mr, mc) = start grid
    go :: Seq.Seq (Int, Int) -> S.HashSet (Int, Int) -> Int -> Int
    go queue seen acc = case Seq.viewl queue of
      EmptyL -> acc
      ((r, c) :< rest) ->
        if S.member (r, c) seen
        then go rest seen acc
        else go (rest >< next) (S.insert (r, c) seen) (acc + 1)
        where
          next = Seq.fromList [(pr, pc) | (pr, pc) <- [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
                                        , not (S.member (pr, pc) seen)
                                        , not (M.member (pr, pc) grid)
                                        ]

vis :: Grid -> String
vis grid = unlines $ [[if M.member (r, c) grid then '#' else '.' | c <- [csMin..csMax]] | r <- [rsMin..rsMax]]
  where rsMax = maximum $ [r | (r, _) <- M.keys grid]
        rsMin = minimum $ [r | (r, _) <- M.keys grid]
        csMax = maximum $ [c | (_, c) <- M.keys grid]
        csMin = minimum $ [c | (_, c) <- M.keys grid]

solve :: Pick -> Challenge -> Int
solve pick input = M.size trench + rest
  where trench = dig (map pick input)
        rest = fill trench

part1 :: Challenge -> String
part1 = show . solve fst

part2 :: Challenge -> String
part2 = show . solve snd

main :: IO ()
main = visual "18" parse part1 part2