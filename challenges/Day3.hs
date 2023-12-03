{-# LANGUAGE MultiWayIf #-}
module Main where

import qualified Data.HashMap.Strict as M
import Data.List (foldl', nub)
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)
import Advent (challenge)
import Utils (readInt)

data Cell = Pointer (Int, Int) | Number (Int, Int) Int | Gear | Symbol

type Diagram = M.HashMap (Int, Int) Cell

type Challenge = Diagram

compact :: [((Int, Int), Char)] -> [((Int, Int), String)]
compact (((r, c), d):rest) = foldl' go [((r, c), [d])] rest
  where
    areDigits = all isDigit
    append a as = as ++ [a]
    go :: [((Int, Int), String)] -> ((Int, Int), Char) -> [((Int, Int), String)]
    go acc@(((brow, bcol), b):more) ((row, col), s)
      | s == '.' = acc
      | brow == row && col == bcol + length b && isDigit s && areDigits b = (((brow, bcol), append s b):more)
      | otherwise = (((row, col), [s]):acc)

n :: (Int, Int) -> String -> [((Int, Int), Cell)]
n (row, col) literal = (((row, col), Number (row, col) (readInt literal)):ps)
  where
    ps = [((row, col + offset), Pointer (row, col)) | offset <- [1..length literal - 1]]

fill :: [((Int, Int), String)] -> [((Int, Int), Cell)]
fill cells = do
  (coord, cell) <- cells
  if | isDigit (head cell) -> (n coord cell)
     | head cell == '*' -> return (coord, Gear)
     | otherwise -> return (coord, Symbol)


parse :: String -> Challenge
parse input = M.fromList $ fill . compact $ do
  (r, line) <- zip [0..] (lines input)
  (c, letter) <- zip [0..] line
  return ((r, c), letter)

neighbors :: Diagram -> (Int, Int) -> [Cell]
neighbors diagram (row, col) = [cell | dc <- [-1, 0, 1], dr <- [-1, 0, 1], Just cell <- [M.lookup (row + dr, col + dc) diagram]]

numberAt :: Diagram -> (Int, Int) -> Maybe Int
numberAt diagram coord = case M.lookup coord diagram of
  Just (Number _ value) -> Just value
  _ -> Nothing

c :: Cell -> Maybe (Int, Int)
c (Pointer p) = Just p
c (Number p _) = Just p
c _ = Nothing

check :: Diagram -> (Int, Int) -> Cell -> Maybe (Int, Int)
check _ _ Symbol = Nothing
check _ _ Gear = Nothing
check diagram coord (Pointer ptr) = if not $ null $ [() | nn <- neighbors diagram coord, Nothing <- [c nn]] then Just ptr else Nothing
check diagram coord (Number _ _) = if not $ null $ [() | nn <- neighbors diagram coord, Nothing <- [c nn]] then Just coord else Nothing

part1 :: Challenge -> Int
part1 diagram = sum $ mapMaybe (numberAt diagram) $ nub $ mapMaybe (uncurry (check diagram)) $ M.toList diagram

check2 :: Diagram -> (Int, Int) -> Cell -> Maybe Int
check2 diagram coord Gear = get $ mapMaybe (numberAt diagram) $ nub $ [ptr | nn <- neighbors diagram coord, Just ptr <- [c nn]]
  where
    get :: [Int] -> Maybe Int
    get [a, b] = Just (a * b)
    get _ = Nothing
check2 _ _ _ = Nothing

part2 :: Challenge -> Int
part2 diagram = sum $ mapMaybe (uncurry (check2 diagram)) $ M.toList diagram

main :: IO ()
main = challenge "3" parse part1 part2