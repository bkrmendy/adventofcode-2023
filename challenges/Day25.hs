module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (sortOn, delete)
import Data.List.Split (splitOn)
import Advent (visual)
import Utils (FIFO, singletonFifo, pushMany, pull)

type Components = M.HashMap String [String]
type Challenge = Components

parse :: String -> Challenge
parse input = M.fromListWith (++) $ do
    line <- lines input
    let [name, cs] = splitOn ": " line
        components = words cs
    c <- components
    [(name, [c]), (c, [name])]

removeEdge :: String -> String -> Components -> Components
removeEdge from to = M.adjust (delete from) to . M.adjust (delete to) from

connectedGroup :: Components -> FIFO String -> S.HashSet String -> S.HashSet String
connectedGroup components queue seen = case pull queue of
  Nothing -> seen
  Just (node, rest) ->
    if S.member node seen then connectedGroup components rest seen
    else connectedGroup components (pushMany (components M.! node) rest) (S.insert node seen)

part1 :: Challenge -> String
part1 graph = show $ size "ddj" * size "rnx" 
  where
    pruned = (removeEdge "ddj" "rnx" . removeEdge "lxb" "vcq" . removeEdge "mmr" "znk") $ graph
    size s = S.size (connectedGroup pruned (singletonFifo s) S.empty)

part2 :: Challenge -> String
part2 = const ""

main :: IO ()
main = visual "25" parse part1 part2