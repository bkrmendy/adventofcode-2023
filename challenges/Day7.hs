module Main where

import Data.List (sort, sortBy, group, elemIndex)
import Data.Bifunctor
import Data.Maybe (fromJust)
import Control.Monad
import Advent (challenge)
import Utils (readInt, replace, count)

type Card = (String, [Int])
type Challenge = [(Card, Int)]

groups :: String -> [Int]
groups = sort . map length . group . sort

parse :: String -> Challenge
parse = map ((\[card, bid] -> ((card, groups $ card), readInt bid)) . words) . lines

rankings :: String -> String -> [Int]
rankings labels = map (fromJust . (`elemIndex` labels))

rating1 :: [Int] -> Int
rating1 [5] = 7
rating1 [1, 4] = 6
rating1 [2, 3] = 5
rating1 [1, 1, 3] = 4
rating1 [1, 2, 2] = 3
rating1 [1, 1, 1, 2] = 2
rating1 _ = 1

ordering :: (([Int], Int), Int) -> (([Int], Int), Int) -> Ordering
ordering ((h1, c1), _) ((h2, c2), _) = case compare c1 c2 of
  EQ -> compare h1 h2
  oo -> oo

result :: [(([Int], Int), Int)] -> String
result = show . sum . map (uncurry (*)) . zip [1..] . map snd

substs :: String -> [String]
substs hand | 'J' `notElem` hand = [hand]
substs hand = concatMap substs $ do
  (i, c) <- zip [0..] hand
  guard $ c == 'J'
  alt <- "23456789TQKA"
  pure $ replace i alt hand

rating2 :: String -> Int
rating2 hand | count 'J' hand == 4 || count 'J' hand == 5 = 5
rating2 hand = maximum $ map (rating1 . groups) $ substs hand


part1 :: Challenge -> String
part1 = result . sortBy ordering . map (first (bimap (rankings "23456789TJQKA") rating1))

part2 :: Challenge -> String
part2 = result . sortBy ordering . map (first (\(card, _) -> ((rankings "J23456789TQKA" card), rating2 card)))

main :: IO ()
main = challenge "7" parse part1 part2