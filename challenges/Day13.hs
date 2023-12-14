module Main where

import Data.List.Split (splitOn)
import Data.List (nub)
import Advent (challenge)
import Utils (transpose, replace)

maybeSymmetric :: [String] -> [String] -> [Int]
maybeSymmetric _ [] = []
maybeSymmetric (a:as) (b:bs) =
  if a == b && all (== True) (zipWith (==) as bs)
  then (length (a:as):maybeSymmetric (b:a:as) bs)
  else maybeSymmetric (b:a:as) bs

type UnSmudging = [String] -> [[String]]

symmetry :: UnSmudging -> [String] -> [Int]
symmetry unsmudging pattern = nub [score | updated <- unsmudging pattern, scores <- [go updated], score <- scores]
  where
    checkSymmetry multiplier (l:ls) = map (* multiplier) $ maybeSymmetric [l] ls
    go p = checkSymmetry 100 p ++ checkSymmetry 1 (transpose p) 

unsmudge :: [String] -> [[String]]
unsmudge p = do
  (r, row) <- zip [0..] p
  (c, cell) <- zip [0..] row
  let opposite = if cell == '.' then '#' else '.' 
  pure $ replace r (replace c opposite row) p

solve :: [[String]] -> (Int, Int)
solve patterns = (sum smudged, sum alts)
  where
    smudged = map (head . symmetry pure) patterns
    desmudged = map (symmetry unsmudge) patterns
    alts = map head $ map f $ zip smudged desmudged
    f (p, ps) = filter (/= p) ps

type Challenge = (Int, Int)

parse :: String -> Challenge
parse = solve . map lines . splitOn "\n\n"

part1 :: Challenge -> String
part1 = show . fst

part2 :: Challenge -> String
part2 = show . snd 

main :: IO ()
main = challenge "13" parse part1 part2