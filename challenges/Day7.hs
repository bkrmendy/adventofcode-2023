{-# LANGUAGE Strict #-}
module Main where

import Data.List (sort, sortOn, group, elemIndex)
import Data.Tuple (swap)
import Data.Bifunctor
import Data.Maybe (fromJust)
import Control.Monad
import Advent (challenge)
import Utils (readInt, replace, count)

type Card = (String, [Int])
type Challenge = [(Card, Int)]

groups :: String -> [Int]
groups = reverse . sort . map length . group . sort

parse :: String -> Challenge
parse = map ((\[card, bid] -> ((card, groups $ card), readInt bid)) . words) . lines

rankings :: String -> String -> [Int]
rankings labels = map (fromJust . (`elemIndex` labels))

rating :: [Int] -> Int
rating [5] = 7
rating [1, 4] = 6
rating [2, 3] = 5
rating [1, 1, 3] = 4
rating [1, 2, 2] = 3
rating [1, 1, 1, 2] = 2
rating _ = 1

result :: [(([Int], Int), Int)] -> String
result = show . sum . map (uncurry (*)) . zip [1..] . map snd

substitutions :: String -> [String]
substitutions hand | 'J' `notElem` hand = [hand]
substitutions hand = concatMap substitutions $ do
  (i, c) <- zip [0..] hand
  guard $ c == 'J'
  alt <- "23456789TQKA"
  pure $ replace i alt hand

rating2 :: String -> Int
rating2 hand | count 'J' hand == 4 || count 'J' hand == 5 = 5
rating2 hand = maximum $ map (rating . groups) $ substitutions hand

part1 :: Challenge -> String
part1 = result . sortOn (swap . fst) . map (first (bimap (rankings "23456789TJQKA") rating))

part2 :: Challenge -> String
part2 = result . sortOn (swap . fst) . map (first (\(card, _) -> ((rankings "J23456789TQKA" card), rating2 card)))

main :: IO ()
main = challenge "7" parse part1 part2