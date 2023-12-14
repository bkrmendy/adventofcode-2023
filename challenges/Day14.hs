module Main where

import Prelude hiding (cycle)
import qualified Data.HashMap.Strict as M
import Data.List (transpose)
import Advent (challenge)

rotateLeft :: [String] -> [String]
rotateLeft = transpose . map reverse

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

type Challenge = [String]

parse :: String -> Challenge
parse = rotateLeft . lines

tilt :: String -> String -> String -> String
tilt space acc [] = reverse (space ++ acc)
tilt space acc ('.':rest) = tilt ('.':space) acc rest
tilt space acc ('O':rest) = tilt space ('O':acc) rest
tilt space acc ('#':rest) = tilt [] ('#':(space ++ acc)) rest

scores :: String -> Int
scores l = go (length l) l
  where go _ [] = 0
        go s ('O':rest) = s + go (s - 1) rest
        go s (_:rest) = go (s - 1) rest

part1 :: Challenge -> String
part1 = show . sum . map (scores . tilt [] [])

cycle :: [String] -> [String]
cycle = go . go . go . go
  where go = rotateRight . map (tilt [] [])

cc :: Int -> ([String] -> [String]) -> [String] -> [String]
cc 0 _ s = s
cc n f s = cc (n - 1) f (f s)

move :: Int -> M.HashMap [String] Int -> Int -> [String] -> [String]
move times seen current board = case M.lookup board seen of
  Nothing -> let next = cycle board in move times (M.insert board current seen) (current + 1) next
  Just start -> let
    period = current - start
    remaining = (times - current) `mod` period
    in cc remaining cycle board

part2 :: Challenge -> String
part2 = show . sum . map scores . move 1000000000 M.empty 0

main :: IO ()
main = challenge "14" parse part1 part2