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

push :: Char -> Maybe [String] -> Maybe [String]
push _ Nothing = Nothing
push c (Just strings) = Just $ map (c:) strings

mor :: Maybe [a] -> Maybe [a] -> Maybe [a]
mor Nothing a = a
mor a Nothing = a
mor (Just a) (Just b) = Just (a ++ b)

type Lookup = M.HashMap (String, [Int]) Result
type Result = Maybe [String]

withMemo :: (String -> [Int] -> S.State Lookup Result) -> String -> [Int] -> S.State Lookup Result
withMemo f s c = do
  memo <- S.gets $ \m -> M.lookup (s, c) m
  case memo of
    Just res -> return res
    Nothing -> do
      res <- f s c
      S.modify' $ M.insert (s, c) res
      return res 

consumeHashesWithMemo, consumeHashes :: String -> [Int] -> S.State Lookup Result
consumeHashesWithMemo = withMemo consumeHashes
consumeHashes [] [0] = return $ Just [[]]
consumeHashes ('#':_) [] = return $ Nothing
consumeHashes ('#':_) (0:_) = return $ Nothing
consumeHashes [] _ = return $ Nothing
consumeHashes ('?':rest) (0:count) = push '.' <$> consume rest count
consumeHashes ('?':rest) (c:count) = push '#' <$> consumeHashesWithMemo rest (c - 1:count)
consumeHashes s (0:rest) = consume s rest
consumeHashes ('#':rest) (c:count) = push '#' <$> consumeHashesWithMemo rest (c - 1:count)
consumeHashes ('.':_) _ = return $ Nothing

consumeHashes p c = error ("Not handled: " ++ p ++ " " ++ show c)

consumeWithMemo, consume :: String -> [Int] -> S.State Lookup Result
consumeWithMemo = withMemo consume
consume [] [] = return $ Just [[]]
consume [] _ = return $ Nothing
consume ('.':rest) counts = push '.' <$> consume rest counts
consume ('#':rest) counts = consumeHashes ('#':rest) counts
consume ('?':rest) counts = mor <$> withHash <*> withDot
  where withDot  = consume ('.':rest) counts
        withHash = consume ('#':rest) counts

consume p c = error ("Not handled: " ++ p ++ " " ++ show c) 

arrangements :: Record -> [String]
arrangements (MkRecord s c) = case S.evalState (consumeWithMemo s c) M.empty  of
  Nothing -> error ("No arrangements for " ++ show s ++ " " ++ show c)
  Just as -> as

part1 :: Challenge -> String
part1 = show . sum . map (length . arrangements)

unfold :: Record -> Record
unfold (MkRecord s c) = MkRecord (intercalate "?" (take 5 $ repeat s)) (concat $ take 5 $ repeat c) 

part2 :: Challenge -> String
part2 = show . sum . map (length . arrangements . unfold)

main :: IO ()
main = challenge "12" parse part1 part2