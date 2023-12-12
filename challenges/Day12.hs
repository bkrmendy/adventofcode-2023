module Main where

import Data.List.Split (splitOn)
import Data.List (intercalate)
import qualified Data.HashMap.Strict as M
import qualified Control.Monad.State as S
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
consumeHashes [] [0] = return $ Just 1
consumeHashes ('#':_) [] = return $ Nothing
consumeHashes ('#':_) (0:_) = return $ Nothing
consumeHashes [] _ = return $ Nothing
consumeHashes ('?':rest) (0:count) = consume rest count
consumeHashes ('?':rest) (c:count) = consumeHashesWithMemo rest (c - 1:count)
consumeHashes s (0:rest) = consume s rest
consumeHashes ('#':rest) (c:count) = consumeHashesWithMemo rest (c - 1:count)
consumeHashes ('.':_) _ = return $ Nothing

consumeHashes p c = error ("Not handled: " ++ p ++ " " ++ show c)

consumeWithMemo, consume :: String -> [Int] -> S.State Lookup Result
consumeWithMemo = withMemo consume
consume [] [] = return $ Just 1
consume [] _ = return $ Nothing
consume ('.':rest) counts = consume rest counts
consume ('#':rest) counts = consumeHashes ('#':rest) counts
consume ('?':rest) counts = add <$> withHash <*> withDot
  where withDot  = consume ('.':rest) counts
        withHash = consume ('#':rest) counts

consume p c = error ("Not handled: " ++ p ++ " " ++ show c) 

arrangements :: Record -> Int
arrangements (MkRecord s c) = case S.evalState (consumeWithMemo s c) M.empty  of
  Nothing -> error ("No arrangements for " ++ show s ++ " " ++ show c)
  Just as -> as

part1 :: Challenge -> String
part1 = show . sum . map (arrangements)

unfold :: Record -> Record
unfold (MkRecord s c) = MkRecord (intercalate "?" (take 5 $ repeat s)) (concat $ take 5 $ repeat c) 

part2 :: Challenge -> String
part2 = show . sum . map (arrangements . unfold)

main :: IO ()
main = challenge "12" parse part1 part2