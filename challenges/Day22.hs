module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (sort, sortOn, nub)
import Data.List.Split (splitOn)
import Advent (visual)
import Utils (readInt, FIFO, pull, pushMany, singletonFifo)

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

supporters :: [Piece] -> M.HashMap Piece [Piece]
supporters pieces = M.fromList $ map go pieces
  where go p = (p, [n | n <- pieces, footprintsOverlap n p, p `isAbove` n])

count :: [[Piece]] -> Int
count sps = length sps - length singleSupports
  where singleSupports = nub $ [p | [p] <- sps]

part1 :: Challenge -> String
part1 = show . count . M.elems . supporters . fall []

supporting :: [Piece] -> M.HashMap Piece [Piece]
supporting pieces = M.fromList $ map go pieces
  where go p = (p, [n | n <- pieces, footprintsOverlap n p, n `isAbove` p])

collapse :: M.HashMap Piece [Piece] -> M.HashMap Piece [Piece] -> S.HashSet Piece -> FIFO Piece -> S.HashSet Piece
collapse upholding restingOn falling q = case pull q of
  Nothing -> falling
  Just (piece, rest) ->
    let
      falling' = S.insert piece falling
      sps = [p | p <- upholding M.! piece, all (\e -> S.member e falling') (restingOn M.! p)]
    in collapse upholding restingOn falling' (pushMany sps rest)

part2 :: Challenge -> String
part2 pieces = show $ sum $ map (collapse1) singleSupports
  where steady = fall [] pieces
        sporrting = supporting steady
        sporrters = supporters steady
        singleSupports = nub $ [p | [p] <- M.elems sporrters]
        collapse1 p = pred $ S.size $ collapse sporrting sporrters S.empty (singletonFifo p)

main :: IO ()
main = visual "22" parse part1 part2