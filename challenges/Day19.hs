module Main where

import qualified Data.HashMap.Strict as M
import Text.Parsec hiding (parse)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Advent (challenge)
import Utils (int, parseL)
import Debug.Trace (traceShow)

data Destination = Accepted | Rejected | Rule String deriving (Show, Eq)
data Test = Constant Destination | Check Char Char Int Destination deriving Show
type Rules = M.HashMap String [Test]

pDestination :: Parsec String () Destination
pDestination = (Accepted <$ char 'A') <|> (Rejected <$ char 'R') <|> (Rule <$> many1 letter)

pTest, pOtherRule, pCheck :: Parsec String () Test
pCheck = do
  prop <- letter
  cmp <- char '<' <|> char '>'
  value <- int
  _ <- char ':'
  dst <- pDestination
  return $ Check prop cmp value dst

pOtherRule = do
  a <- letter
  rest <- many1 letter
  return $ Constant (Rule (a:rest))

pTest = ((Constant $ Accepted) <$ char 'A')
     <|> ((Constant $ Rejected) <$ char 'R')
     <|> try pOtherRule
     <|> pCheck

pRule :: Parsec String () (String, [Test])
pRule = do
  name <- many1 letter
  _ <- char '{'
  tests <- sepBy pTest (char ',')
  _ <- char '}'
  return (name, tests)

data Part = MkPart { _x :: Int, _m :: Int, _a :: Int, _s :: Int } deriving Show

pPart :: Parsec String () Part
pPart = do
  _ <- string "{x="
  x <- int
  _ <- string ",m="
  m <- int
  _ <- string ",a="
  a <- int
  _ <- string ",s="
  s <- int
  _ <- string "}"
  return $ MkPart x m a s


type Challenge = (Rules, [Part])

--dest :: Test -> Destination
--dest (Constant d) = d
--dest (Check _ _ _ d) = d
--
--same :: [Test] -> Maybe [Test]
--same (t:t2:rest) = if all (\c -> dest c == dest t) (t2:rest) then Just [Constant (dest t)] else Nothing
--same [_] = Nothing
--
--constant :: Rules -> [Test] -> Maybe [Test]
--constant rules (:rest) 


--samesIter :: Rules -> Rules
--samesIter rules = traceShow (length next, M.size rules) $ if null next then rules else samesIter updated
--  where next = [(name, common) | (name, tests) <- M.toList rules
--                                        , Just common <- [same tests]]
--        updated = foldl' (\r (n, t) -> M.insert n t r) rules next
        
parse :: String -> Challenge
parse input = (rules, parts)
  where [rulesRaw, partsRaw] = splitOn "\n\n" input
        rules = M.fromList $ parseL pRule rulesRaw
        parts = parseL pPart partsRaw

get :: Char -> Part -> Int
get 'x' (MkPart x _ _ _) = x
get 'm' (MkPart _ m _ _) = m
get 'a' (MkPart _ _ a _) = a
get 's' (MkPart _ _ _ s) = s

check :: Char -> Char -> Int -> Part -> Bool
check prop '<' val part = get prop part < val
check prop '>' val part = get prop part > val

evaluate :: Rules -> Part -> Bool
evaluate rules part = go (rules M.! "in")
  where
        step Accepted = True
        step Rejected = False
        step (Rule name) = go (rules M.! name)
        go ((Constant dst):_) = step dst
        go ((Check prop test val dst):rest)
          | check prop test val part = step dst
          | otherwise = go rest

rating :: Part -> Int
rating (MkPart x m a s) = x + m + a + s

part1 :: Challenge -> String
part1 (rules, parts) = show $ sum $ map rating $ filter (evaluate rules) parts

part2 :: Challenge -> String
part2 (rules, _) = show $ sum $ map rating $ filter (evaluate rules) parts
  where parts = [MkPart x m a s | x <- [1..4000], m <- [1..4000], a <- [1..4000], s <- [1..4000]]

main :: IO ()
main = challenge "19" parse part1 part2