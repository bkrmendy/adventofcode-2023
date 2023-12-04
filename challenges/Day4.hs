module Main where

import qualified Data.HashSet as S
import qualified Text.Parsec as P
import qualified Data.Array.MArray as A
import Data.Array.ST (STArray)
import Control.Monad
import Control.Monad.ST
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

part1 :: Challenge -> Int
part1 cards = sum $ [2 ^ (s - 1) | c <- cards, let s = score c, s > 0]

part2 :: Challenge -> Int
part2 cards = runST $ do
  arr <- A.newArray (1, length scores) 1 :: ST s (STArray s Int Int)
  forM_ scores $ \(i, s) -> do
    forM_ [i+1..i+s] $ \ix -> do
      v1 <- A.readArray arr i
      v2 <- A.readArray arr ix
      A.writeArray arr ix (v1 + v2)
  es <- A.getElems arr
  return $ sum es
  where
    scores = zip [1..] $ map score cards

main :: IO ()
main = challenge "4" parse part1 part2