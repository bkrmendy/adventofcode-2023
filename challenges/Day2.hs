module Main where

import Prelude hiding (round)
import Advent (challenge)
import Data.List (foldl')
import qualified Text.Parsec as P
import Utils (int, parseL)

data Round = MkRound { _green :: Maybe Int, _blue :: Maybe Int, _red :: Maybe Int } deriving (Show)

data Game = MkGame { _id :: Int, _rounds :: [Round] } deriving (Show)

listToMaybe :: ([a] -> Maybe a) -> [a] -> Maybe a
listToMaybe _ [] = Nothing
listToMaybe f l = f l

round :: P.Parsec String () Round
round = do
  cubes <- P.sepBy1 p (P.string ", ")
  let
    red = listToMaybe (Just . head) [n | (n, c) <- cubes, c == "red"]
    green = listToMaybe (Just . head) [n | (n, c) <- cubes, c == "green"]
    blue = listToMaybe (Just . head)  [n | (n, c) <- cubes, c == "blue"]
  return MkRound { _red = red, _green = green, _blue = blue }
  where
    p = (,) <$> (int <* P.space) <*> (P.many1 P.letter)

game :: P.Parsec String () Game
game = MkGame <$> (P.string "Game " *> int <* P.string ": ") <*> (P.sepBy round (P.string "; "))

type Challenge = [Game]

parse :: String -> Challenge
parse = parseL game

part1 :: Challenge -> Int
part1 = sum . (map _id) . filter (all valid . _rounds)
  where
    nothingOrLE m = maybe True (m >=) 
    valid (MkRound green blue red) = 13 `nothingOrLE` green && 14 `nothingOrLE` blue && 12 `nothingOrLE` red

part2 :: Challenge -> Int
part2 = sum . map (power . foldl' fewestPossible (0, 0, 0) . _rounds)
  where
    power (g, b, r) = g * b * r
    
    nothingOrMax n m = maybe m (`max` m) n 
    
    fewestPossible :: (Int, Int, Int) -> Round -> (Int, Int, Int)
    fewestPossible (g, b, r) (MkRound green blue red) = (green `nothingOrMax` g, blue `nothingOrMax` b, red `nothingOrMax` r)
    
main :: IO ()
main = challenge "2" parse part1 part2