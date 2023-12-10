module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (nub)
import Advent (challenge)
import Utils ()

type Tiles = M.HashMap (Int, Int) Char
type Trail = S.HashSet (Int, Int)
type Challenge = Tiles

parse :: String -> Challenge
parse input = M.fromList $ do
  (iRow, row) <- zip [0..] $ lines input
  (iCol, cell) <- zip [0..] $ row
  pure ((iRow, iCol), cell)

step :: Tiles -> (Int, Int) -> (Int, Int) -> (Int, Int)
step tiles (pRow, pCol) (row, col) = case tiles M.! (row, col) of
  '-' -> (row, col + dCol)
  '|' -> (row + dRow, col)
  'L' -> if dRow == 0 then (row - 1, col) else (row, col + 1)
  'J' -> if dRow == 0 then (row - 1, col) else (row, col - 1)
  '7' -> if dRow == 0 then (row + 1, col) else (row, col - 1)
  'F' -> if dRow == 0 then (row + 1, col) else (row, col + 1)
  where (dRow, dCol) = (row - pRow, col - pCol)

walk :: Tiles -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
walk tiles prevPos pos = if tile == 'S' then [pos, nextPos] else (pos:walk tiles pos nextPos)
  where nextPos = step tiles prevPos pos
        tile = tiles M.! nextPos

loop :: Challenge -> [(Int, Int)]
loop tiles = walk tiles (sRow, sCol) (sRow, sCol - 1)
  where (sRow, sCol) = head $ [p | (p, s) <- M.toList tiles, s == 'S']

farthest :: [(Int, Int)] -> Int
farthest steps = distances !! (length distances `div` 2)
  where distances = scanl (\d _ -> d + 1) 0 steps

part1 :: Challenge -> String
part1 = show . farthest . loop


enclosed :: Tiles -> Int -> Int
enclosed tiles row = go 0 False (row, 0)
  where
    go acc inside (r, c) =
      case M.lookup (r, c) tiles of
        Nothing -> acc
        Just cell ->
          let
            next = if cell == '.' && inside then acc + 1 else acc
            nextInside = if cell `elem` "|JLS" then not inside else inside
          in go next nextInside (r, c + 1)

part2 :: Challenge -> String
part2 tiles = show $ sum $ map (enclosed scrubbed) rows
  where rows = nub $ map fst $ M.keys tiles
        trail = S.fromList $ loop tiles
        scrubbed = M.mapWithKey (\cell value -> if S.member cell trail then value else '.') tiles

main :: IO ()
main = challenge "10" parse part1 part2