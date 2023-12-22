module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (sortOn, foldl')
import Data.List.Split (splitOn)
import Advent (visual)
import Utils (readInt)
import Debug.Trace (traceShow)

type Piece = (Int, (Int, Int, Int), (Int, Int, Int))
type Challenge = [Piece]

parse :: String -> Challenge
parse = sortOn (\(_, (_, _, fz), (_, _, tz)) -> (fz, tz)) . map (\(s, line) -> p s $ splitOn "~" line) . zip [1..] . lines
  where p s [f, t] = (s, c (splitOn "," f), c (splitOn "," t))
        c [x, y, z] = (readInt x, readInt y, readInt z)

type FallState = M.HashMap (Int, Int) (Int, Int) -- piece id, height
type PieceDependency = (Int, [Int]) -- piece id, rests on pieces

supportingCubes :: FallState -> [(Int, Int)] -> (Int, [Int])
supportingCubes state footprint = (maxZ, supports)
  where zs = [M.lookupDefault (0, 0) (x, y) state | (x, y) <- footprint]
        maxZ = maximum (map snd zs)
        supports = [i | (i, z) <- zs, z == maxZ]

fall :: FallState -> [Piece] -> [PieceDependency]
fall _ [] = []
fall state (piece:rest)= (s, supports):fall nextState rest
  where
    (s, (fx, fy, fz), (tx, ty, tz)) = piece
    height = length [fz..tz]
    footprint = [(x, y) | x <- [fx..tx], y <- [fy..ty]]
    (maxZFromSupports, supports) = supportingCubes state footprint
    bottomZ = maxZFromSupports + 1
    topZ = bottomZ + height
    nextState = foldl' (\m c -> M.insert c (s, topZ) m) state footprint

count :: [PieceDependency] -> Int
count deps = length piecesToRemove
  where
    singleSupportPieces = S.fromList [i | [i] <- map snd deps, i /= 0]
    piecesToRemove = [i | i <- map fst deps, not (S.member i singleSupportPieces), i /= 0]

part1 :: Challenge -> String
part1 = show . count . fall M.empty

part2 :: Challenge -> String
part2 = const "-"

main :: IO ()
main = visual "22" parse part1 part2