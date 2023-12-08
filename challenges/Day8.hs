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
step dir m node = next $ m M.! node
  where next = if dir == 'L' then fst else snd

walk :: (String -> Bool) -> String -> Map -> String -> Int -> Int
walk stop _        _ loc stepsSoFar | stop loc = stepsSoFar
walk stop (d:dirs) m loc stepsSoFar = walk stop dirs m (step d m loc) (stepsSoFar + 1)

leastCommonMultiple :: [Int] -> Int
leastCommonMultiple = foldr lcm 1

solve :: (String -> Bool) -> (String -> Bool) -> String -> Map -> Int
solve isStartNode isEndNode dirs m = leastCommonMultiple $ do
  startNode <- filter isStartNode (M.keys m)
  pure $ walk isEndNode dirs m startNode 0

part1 :: Challenge -> String
part1 = show . uncurry (solve (== "AAA") (== "ZZZ"))

part2 :: Challenge -> String
part2 = show . uncurry (solve ((== 'A') . last) ((== 'Z') . last))

main :: IO ()
main = challenge "8" parse part1 part2