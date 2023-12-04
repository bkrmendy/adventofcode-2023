module Main where

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import qualified Text.Parsec as P
import Data.List (foldl')
import Advent (challenge)
import Utils (int, parseLines)

data Card = MkCard { _id :: Int, _winning :: S.HashSet Int, _valid :: S.HashSet Int } deriving (Show)

type Challenge = [Card]

card :: P.Parsec String () Card
card = MkCard <$> (P.string "Card" *> P.spaces *> int <* P.string ": " <* P.spaces)
              <*> (S.fromList <$> P.many1 (int <* P.spaces))
              <*> (P.char '|' *> P.spaces *> (S.fromList <$> P.many1 (int <* P.spaces)))

parse :: String -> Challenge
parse = map (parseLines card) . lines

score :: Card -> Int
score (MkCard _ winning valid) = S.size $ S.intersection winning valid

value :: Int -> Int
value 0 = 0
value n = 2 ^ (n - 1)

part1 :: Challenge -> Int
part1 = sum . map (value . score)

type Counts = M.HashMap Int Int

update :: Counts -> Int -> [Int] -> Counts
update cs i is = foldl' go cs is
  where go cs' ix = M.insertWith (+) (i + ix) (cs M.! i) cs'

part2 :: Challenge -> Int
part2 cards = sum $ foldl' go counts $ [(i, score c) | (i, c) <- zip [1..] cards]
  where
    counts = M.fromList [(i, 1) | (MkCard i _ _) <- cards]
    go :: Counts -> (Int, Int) -> Counts
    go cs (i, ms) = update cs i [1..ms]

main :: IO ()
main = challenge "4" parse part1 part2