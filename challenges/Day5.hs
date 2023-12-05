{-# LANGUAGE MultiWayIf #-}
module Main where

import Data.List.Split (splitOn, chunksOf)
import Data.List (sortOn, foldl')
import Advent (challenge)
import Utils (readInt)

type Mapping = [(Int, Int, Int)]
type Challenge = ([Int], [Mapping])

parseMapping :: String -> Mapping
parseMapping = sortOn (\(_, s, _) -> s) . map (toMapping . map readInt . words) . drop 1 . lines
  where toMapping [d, s, r] = (d, s, s + r -1)
  
parse :: String -> Challenge
parse input =
  let
    (seeds:maps) = splitOn "\n\n" input
    s = map readInt $ drop 1 $ words seeds
    ms = map parseMapping maps
  in (s, ms)
  
lookupMapping :: (Int, Int) -> Mapping -> ((Int, Int), Maybe (Int, Int))
lookupMapping (lo, hi) mapping = withDefault ((lo, hi), Nothing) $ do
  (dest, sLow, sHigh) <- mapping
  if | sLow <= lo && hi <= sHigh -> pure $ (remap sLow dest (lo, hi), Nothing)
     | sLow <= lo && lo <= sHigh ->
      let
        ((l1, h1), (l2, h2)) = split sHigh (lo, hi)
       in pure $ (remap sLow dest (l1, h1), Just (l2, h2))
     | otherwise -> []
  where
    withDefault d ds = if null ds then d else head ds
    remap source dest (l, h) = (dest + l - source, dest + h - source)
    split at (l, h) = ((l, at), (at + 1, h))
    
fanOutMappings :: [(Int, Int)] -> Mapping -> [(Int, Int)]
fanOutMappings [] _ = [] 
fanOutMappings (first:rest) m = case lookupMapping first m of
  (mapped, Nothing) -> (mapped:fanOutMappings rest m)
  (mapped, Just remaining) -> (mapped:fanOutMappings (remaining:rest) m)  

findMapping :: [Mapping] -> (Int, Int) -> [(Int, Int)]
findMapping ms s = foldl' fanOutMappings [s] ms

minimumRange :: [(Int, Int)] -> Int
minimumRange = minimum . map fst

part1 :: Challenge -> Int
part1 (seeds, mappings) = minimumRange $ concatMap (findMapping mappings) ranges
  where ranges = [(a, a) | a <- seeds]

part2 :: Challenge -> Int
part2 (seeds, mappings) = minimumRange $ concatMap (findMapping mappings) $ ranges
  where ranges = map (\[a, b] -> (a, a + b - 1)) $ chunksOf 2 seeds

main :: IO ()
main = challenge "5" parse part1 part2