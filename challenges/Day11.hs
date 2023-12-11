module Main where

import Data.Bifunctor (first, second)
import qualified Data.Sequence as Seq
import qualified Data.HashSet as S
import Data.Sequence (ViewL(EmptyL, (:<)), (><))
import qualified Data.HashMap.Strict as M

import Advent (challenge)

data Cell = Galaxy | Empty (Int, Int) deriving (Eq) -- height, width

type Challenge = [[Cell]]

fix :: String -> [[Cell]]
fix = map (map f) . lines
  where f c = if c == '#' then Galaxy else Empty (1, 1)

parse :: String -> Challenge
parse = fix

cell :: a -> ((Int, Int) -> a) -> Cell -> a
cell g e cel = case cel of
  Galaxy -> g
  Empty d -> e d

mapAllEmpties :: ((Int, Int) -> (Int, Int)) -> [Cell] -> [Cell]
mapAllEmpties f cells = go cells []
  where go [] acc = reverse acc
        go (Galaxy:_) _ = cells
        go (Empty d:rest) acc = go rest (Empty (f d):acc)

expandRows, expandCols :: Int -> [[Cell]] -> [[Cell]]
expandRows by = map (mapAllEmpties (first (* by)))
expandCols _  rs | all null rs = rs
expandCols by rs = zipWith (:) e rest
  where e = mapAllEmpties (second (* by)) (map head rs)
        rest = expandCols by (map tail rs)
        
expand :: Int -> [[Cell]] -> [[Cell]]
expand by = expandCols by . expandRows by

type Chart = M.HashMap (Int, Int) Cell

toChart :: Int -> [[Cell]] -> Chart
toChart by input = M.fromList $ do
  (r, row) <- zip [0..] $ expand by input
  (c, cel) <- zip [0..] row
  pure ((r, c), cel)

down, up, left, right :: Chart -> (Int, Int) -> Maybe ((Int, Int), Int)
down chart (r, c) = (,) (r -1, c) <$> cell 1 fst <$> M.lookup (r - 1, c) chart
up chart (r, c) = (,) (r + 1, c) <$> cell 1 fst <$> M.lookup (r + 1, c) chart
left chart (r, c) = (,) (r, c -1 ) <$> cell 1 snd <$> M.lookup (r, c - 1) chart
right chart (r, c) = (,) (r, c + 1) <$> cell 1 snd <$> M.lookup (r, c + 1) chart

bfs :: Chart -> Seq.Seq ((Int, Int), Int) -> S.HashSet (Int, Int) -> [Int]
bfs chart queue seen = case Seq.viewl queue of
  EmptyL -> []
  ((pos, dist) :< rest) -> if S.member pos seen
    then bfs chart rest seen
    else if chart M.! pos == Galaxy then dist:bfs chart (rest >< neighbors) nextSeen
    else bfs chart (rest >< neighbors) nextSeen
    where nextSeen = S.insert pos seen
          neighbors = Seq.fromList [(n, dist + d) | Just (n, d) <- [down chart pos, up chart pos, left chart pos, right chart pos], not $ S.member n seen, M.member n chart]

shortest :: Chart -> Int
shortest chart = sum kludge `div` 2
  where
    shortests start = bfs chart (Seq.singleton (start, 0)) S.empty
    kludge = concat [shortests (r, c) | ((r, c), cel) <- M.toList chart, cel == Galaxy]

part1 :: Challenge -> String
part1 = show . shortest . toChart 2

part2 :: Challenge -> String
part2 = show . shortest . toChart 1000000

main :: IO ()
main = challenge "11" parse part1 part2