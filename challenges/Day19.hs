module Main where

import qualified Data.HashMap.Strict as M
import Text.Parsec hiding (parse)
import Data.List.Split (splitOn)
import Advent (challenge)
import Utils (int, parseL)

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

data Part a = MkPart { _x :: a, _m :: a, _a :: a, _s :: a } deriving Show

pPart :: Parsec String () (Part Int)
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


type Challenge = (Rules, [Part Int])

parse :: String -> Challenge
parse input = (rules, parts)
  where [rulesRaw, partsRaw] = splitOn "\n\n" input
        rules = M.fromList $ parseL pRule rulesRaw
        parts = parseL pPart partsRaw

get :: Char -> Part a -> a
get 'x' (MkPart x _ _ _) = x
get 'm' (MkPart _ m _ _) = m
get 'a' (MkPart _ _ a _) = a
get 's' (MkPart _ _ _ s) = s

check :: Char -> Char -> Int -> Part Int -> Bool
check prop '<' val part = get prop part < val
check prop '>' val part = get prop part > val

evaluate :: Rules -> Part Int -> Bool
evaluate rules part = go (rules M.! "in")
  where
        step Accepted = True
        step Rejected = False
        step (Rule name) = go (rules M.! name)
        go ((Constant dst):_) = step dst
        go ((Check prop test val dst):rest)
          | check prop test val part = step dst
          | otherwise = go rest

rating :: Part Int -> Int
rating (MkPart x m a s) = x + m + a + s

part1 :: Challenge -> String
part1 (rules, parts) = show $ sum $ map rating $ filter (evaluate rules) parts

split :: (Int, Int) -> Char -> Int -> ((Int, Int), (Int, Int))
split (lo, hi) '<' val = ((lo, val), (val, hi))
split (lo, hi) '>' val = ((val + 1, hi), (lo, val + 1))

type PPart = Part (Int, Int)

splitPart :: PPart -> Char -> Char -> Int -> (PPart, PPart)
splitPart (MkPart x m a s) prop op val = case prop of
  'x' -> let (use, rest) = split x op val in (MkPart use m a s, MkPart rest m a s)
  'm' -> let (use, rest) = split m op val in (MkPart x use a s, MkPart x rest a s)
  'a' -> let (use, rest) = split a op val in (MkPart x m use s, MkPart x m rest s)
  's' -> let (use, rest) = split s op val in (MkPart x m a use, MkPart x m a rest)

cardinality :: PPart -> Int
cardinality (MkPart x m a s) = product $ map (\(l, h) -> h - l) [x, m, a, s]

rangesFor :: Rules -> PPart -> [Test] -> Int
rangesFor _     _      [(Constant Rejected)] = 0
rangesFor _     part   [(Constant Accepted)] = cardinality part
rangesFor rules part   [(Constant (Rule rule))] = rangesFor rules part (rules M.! rule)
rangesFor rules part  ((Check p t v dst):rest) =
  let (use, cont) = splitPart part p t v in
  case dst of
    Rejected -> rangesFor rules cont rest
    Accepted -> (cardinality use) + rangesFor rules cont rest
    Rule rule -> rangesFor rules use (rules M.! rule) + rangesFor rules cont rest

part2 :: Challenge -> String
part2 (rules, _) = show $ parts
  where parts = rangesFor rules (MkPart (1, 4001) (1, 4001) (1, 4001) (1, 4001)) (rules M.! "in")

main :: IO ()
main = challenge "19" parse part1 part2