module Main where

import qualified Data.Map.Strict as M
import Advent (challenge)
import Utils (readInt)
import Numeric (readHex)

type Grid = M.Map Int (M.Map Int Int)
type Pick = ((String, Int), (String, Int)) -> (String, Int)
type Challenge = [((String, Int), (String, Int))]

parseP2Instr :: String -> (String, Int)
parseP2Instr [_, '#', a, b, c, d, e, dn, _] = (dir dn, rh [a,b,c,d,e])
  where
    rh :: String -> Int
    rh i = fst $ head $ (readHex i)
    dir :: Char -> String
    dir '0' = "R"
    dir '1' = "D"
    dir '2' = "L"
    dir '3' = "U"

parse :: String -> Challenge
parse = map p . lines
  where
    p1 d l = (d, readInt l)
    p line = (\[d, l, c] -> (p1 d l, parseP2Instr c)) . words $ line

vertices :: [(String, Int)] -> [(Int, Int)]
vertices = move (0, 0)
  where move v [] = [v]
        move (r, c) (("R", l):rest) = let next = (r, c + l) in next:move next rest
        move (r, c) (("L", l):rest) = let next = (r, c - l) in next:move next rest
        move (r, c) (("U", l):rest) = let next = (r + l, c) in next:move next rest
        move (r, c) (("D", l):rest) = let next = (r - l, c) in next:move next rest

-- | shoelace + pick's
-- | TIL
shoelace :: [(String, Int)] -> Int
shoelace instrs = boundary + inside
  where
    vs = vertices instrs
    determinant (a, b) (c, d) = a * d - b * c
    area = sum $ map (uncurry determinant) $ zip vs (tail vs ++ [head vs])
    boundary = sum $ map snd instrs
    inside = (abs area) `div` 2 - boundary `div` 2 + 1

solve :: Pick -> Challenge -> Int
solve pick = shoelace . map pick

part1 :: Challenge -> String
part1 = show . solve fst

part2 :: Challenge -> String
part2 = show . solve snd

main :: IO ()
main = challenge "18" parse part1 part2