module Main where

import Data.List.Split (splitOn)
import Data.List (intercalate)
import qualified Data.HashMap.Strict as M
import qualified Control.Monad.State as S
import Data.Maybe (fromJust)
import Advent (challenge)
import Utils (readInt)

data Record = MkRecord String [Int] deriving Show

type Challenge = [Record]

parse :: String -> Challenge
parse = map parseLine . lines
  where parseLine line = let [c, g] = splitOn " " line in MkRecord c (map readInt (splitOn "," g))

add :: Maybe Int -> Maybe Int -> Maybe Int
add Nothing a = a
add a Nothing = a
add (Just a) (Just b) = Just (a + b)

type Result = Maybe Int
type Lookup = M.HashMap (Int, [Int]) Result

withMemo :: (String -> [Int] -> S.State Lookup Result) -> String -> [Int] -> S.State Lookup Result
withMemo f s c = do
  memo <- S.gets $ \m -> M.lookup (length s, c) m
  case memo of
    Just res -> return res
    Nothing -> do
      res <- f s c
      S.modify' $ M.insert (length s, c) res
      return res 

consumeHashesWithMemo, consumeHashes :: String -> [Int] -> S.State Lookup Result
consumeHashesWithMemo = withMemo consumeHashes

-- we reached the end OK 
consumeHashes [] [0] = return $ Just 1

-- too many hashes (either no groups remaining or expected fewer hashes)
consumeHashes ('#':_) [] = return $ Nothing
consumeHashes ('#':_) (0:_) = return $ Nothing

-- too few hashes
consumeHashes [] _ = return $ Nothing

-- the last # is followed by a ?
consumeHashes ('?':rest) (0:count) = consume rest count

-- there's a ? among the #s, we treat it as a #
consumeHashes ('?':rest) (c:count) = consumeHashesWithMemo rest (c - 1:count)
consumeHashes s (0:rest) = consume s rest

-- pop the next hash in the run
consumeHashes ('#':rest) (c:count) = consumeHashesWithMemo rest (c - 1:count)

-- expected hash, encountered dot
consumeHashes ('.':_) _ = return $ Nothing

consumeWithMemo, consume :: String -> [Int] -> S.State Lookup Result
consumeWithMemo = withMemo consume

-- we reached the end OK
consume [] [] = return $ Just 1

-- we reached the end without consuming all groups
consume [] _ = return $ Nothing

-- skip dots
consume ('.':rest) counts = consume rest counts

-- consume this hash run
consume ('#':rest) counts = consumeHashes ('#':rest) counts

-- fork: try with the ? replaced to either # or .
consume ('?':rest) counts = add <$> withHash <*> withDot
  where withDot  = consume ('.':rest) counts
        withHash = consume ('#':rest) counts

arrangements :: Record -> Int
arrangements (MkRecord s c) = fromJust $ S.evalState (consumeWithMemo s c) M.empty 

part1 :: Challenge -> String
part1 = show . sum . map (arrangements)

unfold :: Record -> Record
unfold (MkRecord s c) = MkRecord (intercalate "?" (take 5 $ repeat s)) (concat $ take 5 $ repeat c) 

part2 :: Challenge -> String
part2 = show . sum . map (arrangements . unfold)

main :: IO ()
main = challenge "12" parse part1 part2