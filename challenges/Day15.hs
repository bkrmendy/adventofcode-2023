module Main where

import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.List (foldl')
import qualified Data.HashMap.Strict as M
import Advent (challenge)
import Utils (readInt)

type Challenge = [String]

parse :: String -> Challenge
parse = splitOn ","

hash :: String -> Int
hash = foldl' (\acc c -> (((acc + ord c) * 17) `rem` 256)) 0

part1 :: Challenge -> String
part1 = show . sum . map hash

type Lenses = [(String, Int)]

addLens :: (String, Int) -> Lenses -> Lenses
addLens lens [] = [lens]
addLens (label, f) ((l, _):rest) | label == l = ((label, f):rest)
addLens lens (l:rest) = l:addLens lens rest

removeLens :: String -> Lenses -> Lenses
removeLens _ [] = []
removeLens label ((l, _):rest) | label == l = rest
removeLens label (l:rest) = l:removeLens label rest

type Boxes = M.HashMap Int Lenses

process :: String -> Boxes -> String -> Boxes
process acc boxes ['-'] = M.adjust (removeLens label) h boxes
  where label = reverse acc
        h = hash label

process acc boxes ('=':len) = M.adjust (addLens (label, readInt len)) h boxes
  where label = reverse acc
        h = hash label

process acc boxes (c:rest) = process (c:acc) boxes rest

focusingPower :: Boxes -> Int
focusingPower boxes = sum $ do
  (boxNumber, lenses) <- M.toList boxes
  (slotNumber, focalLength) <- zip [1..] (map snd lenses)
  return $ (1 + boxNumber) * slotNumber * focalLength

part2 :: Challenge -> String
part2 = show . focusingPower . foldl' (process []) boxes
  where boxes = M.fromList [(i, []) | i <- [0..255]]

main :: IO ()
main = challenge "15" parse part1 part2