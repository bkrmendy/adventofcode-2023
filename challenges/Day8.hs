module Main where

import qualified Text.Parsec as P
import qualified Data.HashMap.Strict as M
import Data.List.Split (splitOn)
import Advent (challenge)
import Utils (parseL)

type Map = M.HashMap String (String, String)
type Challenge = (String, Map)

parseMapEntry :: P.Parsec String () (String, (String, String))
parseMapEntry = (,) <$> P.many1 P.alphaNum <* P.string " = ("
                    <*> ((,) <$> P.many1 P.alphaNum <* P.string ", "
                             <*> P.many1 P.alphaNum <* P.string ")")

parseMap :: String -> Map
parseMap = M.fromList . parseL (parseMapEntry)

parse :: String -> Challenge
parse input = let [dirs, m] = splitOn "\n\n" input in (cycle dirs, parseMap m)

step :: Char -> Map -> String -> String
step dir m node = next
  where (left, right) = m M.! node
        next = if dir == 'L' then left else right

walk :: (String -> Bool) -> String -> Map -> String -> Int -> Int
walk stop _        _ loc stepsSoFar | stop loc = stepsSoFar
walk stop (d:dirs) m loc stepsSoFar = walk stop dirs m (step d m loc) (stepsSoFar + 1)

part1 :: Challenge -> String
part1 (dirs, m) = show $ walk (== "ZZZ") dirs m "AAA" 0

isStartNode, isEndNode :: String -> Bool
isStartNode node = last node == 'A'
isEndNode   node = last node == 'Z'

leastCommonMultiple :: [Int] -> Int
leastCommonMultiple [x] = x
leastCommonMultiple [a, b] = lcm a b
leastCommonMultiple (x:xs) = lcm x (leastCommonMultiple xs)

part2 :: Challenge -> String
part2 (dirs, m) = show $ leastCommonMultiple x
  where x = [steps | startNode <- filter isStartNode (M.keys m), let steps = walk isEndNode dirs m startNode 0]

main :: IO ()
main = challenge "8" parse part1 part2