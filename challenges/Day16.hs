{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL(EmptyL, (:<)), (><))
import Advent (challenge)
import Utils ()

type Grid = M.HashMap (Int, Int) Char
type Challenge = (Int, Int, Grid)

parse :: String -> Challenge
parse input = (length (head ls), length ls, ) $ M.fromList $ do
  (iRow, row) <- zip [0..] (lines input)
  (iCol, cell) <- zip [0..] row
  pure $ ((iRow, iCol), cell)
  where ls = lines input

data Beam = MkBeam { _pos :: (Int, Int),  _dir :: (Int, Int) }

reflect :: Char -> (Int, Int) -> [(Int, Int)]
reflect '.' dir = [dir]
reflect '|' (dr, dc)
  | dc == 0 = [(dr, dc)]
  | otherwise = [(-1, 0), (1, 0)]
reflect '-' (dr, dc)
  | dr == 0 = [(dr, dc)]
  | otherwise = [(0, -1), (0, 1)]
reflect '/' (dr, dc)
  | dr ==  1 = [( 0, -1)]
  | dr == -1 = [( 0,  1)]
  | dc ==  1 = [(-1,  0)]
  | dc == -1 = [( 1,  0)]
reflect '\\' (dr, dc)
  | dr ==  1 = [( 0,  1)]
  | dr == -1 = [( 0, -1)]
  | dc ==  1 = [( 1,  0)]
  | dc == -1 = [(-1,  0)]

step :: Grid -> Beam -> [Beam]
step grid (MkBeam (r, c) (dr, dc)) = case M.lookup nextPos grid of
  Nothing -> []
  Just cell -> map (MkBeam nextPos) (reflect cell (dr, dc))
  where nextPos = (r + dr, c + dc)

walk :: Grid -> Seq.Seq Beam -> S.HashSet ((Int, Int), (Int, Int)) -> [(Int, Int)]
walk grid queue seen = case Seq.viewl queue of
  EmptyL -> []
  beam@(MkBeam pos dir) :< rest ->
    if S.member (pos, dir) seen
    then walk grid rest (S.insert (pos, dir) seen)
    else pos:walk grid (rest >< next) (S.insert (pos, dir) seen)
    where next = Seq.fromList (step grid beam)

energize :: Grid -> Beam -> [(Int, Int)]
energize grid beam = walk grid (Seq.singleton beam) S.empty

part1 :: Challenge -> String
part1 (_, _, grid) = show $ pred $ S.size $ S.fromList $ energize grid (MkBeam (0, -1) (0, 1))

part2 :: Challenge -> String
part2 (rows, cols, grid) = show $ pred $ maximum $ do
  (pos, dir) <- fromLeft ++ fromRight ++ fromBottom ++ fromTop
  pure $ S.size $ S.fromList $ energize grid (MkBeam pos dir)
  where fromLeft =  [((row, -1), (0, 1)) | row <- [0..rows - 1]]
        fromRight = [((row, cols), (0, -1)) | row <- [0..rows - 1]]
        fromBottom = [((rows, col), (-1, 0)) | col <- [0..cols - 1]]
        fromTop = [((-1, col), (1, 0)) | col <- [0..cols - 1]]
  

main :: IO ()
main = challenge "16" parse part1 part2