module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (sort, sortOn, nub, delete)
import Data.List.Split (splitOn)
import Advent (visual)
import Utils (readInt)
import Debug.Trace (traceShow)

type Piece = ((Int, Int), (Int, Int), (Int, Int))
type Challenge = [Piece]

parse :: String -> Challenge
parse = sortOn (\(_, _, z) -> z) . map (p . splitOn "~") . lines
  where
    p [f, t] =
      let
        ([fx, fy, fz], [tx, ty, tz]) = ((splitOn "," f), (splitOn "," t))
      in ((readInt fx, readInt tx), (readInt fy, readInt ty), (readInt fz, readInt tz))

intersects :: (Int, Int) -> (Int, Int) -> Bool
intersects a b = let [(l1, h1), (l2, h2)] = sort [a, b] in l1 <= h2 && l2 <= h1

footprintsOverlap :: Piece -> Piece -> Bool
footprintsOverlap (x, y, _) (xx, yy, _) = intersects x xx && intersects y yy

fall :: [Piece] -> [Piece] -> [Piece]
fall acc [] = acc
fall acc (piece:rest) = fall ((go acc piece 1):acc) rest
  where
    go [] (x, y, (lz, hz)) maxZ = let drp = lz - maxZ in (x, y, (lz - drp, hz - drp))
    go (a@(_, _, az):aa) p maxZ
      | footprintsOverlap p a = go aa p (max maxZ (snd az + 1))
      | otherwise = go aa p maxZ

isAbove :: Piece -> Piece -> Bool
(_, _, (lz, _)) `isAbove` (_, _, (_, hz)) = lz == hz + 1

supporters :: [Piece] -> M.HashMap Int [Piece]
supporters pieces = M.fromList $ map go (zip [0..] $ pieces)
  where go (i, p) = (i, [n | n <- pieces, footprintsOverlap n p, p `isAbove` n])

count :: M.HashMap Int [Piece] -> Int
count sps = M.size sps - length singleSupports
  where singleSupports = nub $ [p | [p] <- M.elems sps]

part1 :: Challenge -> String
part1 = show . count . supporters . fall []

part2 :: Challenge -> String
part2 = const "-"

main :: IO ()
main = visual "22" parse part1 part2