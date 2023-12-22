module Utils where

import Text.Parsec as Parsec
import Control.Monad (zipWithM)
import Data.Char (digitToInt)
import Data.List (foldl', nub)
import Data.Word8
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

-- | LISTS
notNull :: Foldable t => t a -> Bool
notNull = not . null

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

ns :: (Show a) => Int -> [a] -> [[a]]
ns n xs
  | length xs < n = error ("Too short: " ++ show xs)
  | length xs == n = [xs]
  | otherwise = take 3 xs : ns n (drop 1 xs)

threes :: (Show a) => [a] -> [[a]]
threes = ns 3

-- ^ https://stackoverflow.com/a/21288092
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n>l then [] else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                             in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

-- ^ https://stackoverflow.com/q/19772427
subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

sum2D :: (Num a) => [[a]] -> a
sum2D xs = sum [sum ys | ys <- xs]

updateAtIndex :: (Num a, Enum a, Eq a) => a -> (c -> c) -> [c] -> [c]
updateAtIndex _ _ []       = []
updateAtIndex 0 f (i:rest) = f i : rest
updateAtIndex i f (e:rest) = e : updateAtIndex (i - 1) f rest

replace :: (Num a, Enum a, Eq a) => a -> c -> [c] -> [c]
replace index element = updateAtIndex index (const element)

concatRep :: Int -> String -> String
concatRep n = concat . replicate n

remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 (_:rest) = rest
remove n (hd:rest) = hd:remove (pred n) rest

insert :: a -> Int -> [a] -> [a]
insert a 0 elems = a:elems
insert _ _ [] = []
insert a n (hd:rest) = hd : insert a (pred n) rest

countWhere :: (a -> Bool) -> [a] -> Int
countWhere f = length . filter f

count :: Eq a => a -> [a] -> Int
count x = countWhere (x ==)

maxIndex :: [Int] -> Int
maxIndex list = Prelude.snd . maximum $ zip list [0..]

allEqual :: (Eq a) => [a] -> Bool
allEqual as = case nub as of
  [_] -> True
  _ -> False 
  
select :: [a] -> [(a,[a])]
select []     = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f (x:xs)
    | not (f x) = x : (takeUntil f xs)
    | otherwise = []

-- | SET
listify :: (Ord a) => (a -> Set.Set a -> Set.Set a) -> [a] -> Set.Set a -> Set.Set a
listify _ [] set = set
listify f (a:as) set = listify f as (f a set)

inserts :: (Ord a) => [a] -> Set.Set a -> Set.Set a
inserts = listify Set.insert

deletes :: (Ord a) => [a] -> Set.Set a -> Set.Set a
deletes = listify Set.delete

toggle :: (Ord a) => a -> Set.Set a -> Set.Set a
toggle x xs | x `Set.member` xs = Set.delete x xs
toggle x xs = Set.insert x xs

-- | MAP

insertL :: (Ord k) => k -> a -> Map.Map k [a] -> Map.Map k [a]
insertL key val m =
  case Map.lookup key m of
    Nothing -> Map.insert key [val] m
    Just elems -> Map.insert key (val:elems) m

ensureE :: (Ord k) => k -> Map.Map k [a] -> Map.Map k [a]
ensureE key m =
  case Map.lookup key m of
    Nothing -> Map.insert key [] m
    Just _ -> m

-- | PARSING
 
readInt :: String -> Int
readInt i = read i :: Int

word :: Parsec String () String
word = many1 letter

-- ^ https://www.schoolofhaskell.com/user/stevely/parsing-floats-with-parsec#parsing-integers-with-leading-sign
int :: Parsec.Parsec String () Int
int = rd <$> (plus <|> minus <|> number)
  where rd     = read :: String -> Int
        plus   = char '+' *> number
        minus  = (:) <$> char '-' <*> number
        number = many1 digit
        
integer :: Parsec.Parsec String () Integer
integer = toInteger <$> int

-- | actual space!!!
space :: Parsec.Parsec String () Char
space = Parsec.char ' '        

parseLines :: Parsec.Parsec String () a -> String -> a
parseLines parser = parseI
  where
    parseI input =
      case Parsec.parse parser "" input of
        Left err -> error $ show err
        Right commands -> commands
        
parseL :: Parsec.Parsec String () a -> String -> [a]
parseL p = parseLines (sepBy1 p newline)

-- ^ adapted from https://stackoverflow.com/a/26961027
fromBinaryString :: String -> Int
fromBinaryString = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- | HASH
md5 :: Bs.ByteString -> Bs.ByteString
md5 = Bs.pack . map toLower . Bs.unpack . B16.encode . MD5.hash

-- | Combinator
s :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
s f g x = f x (g x)

-- | MATH

xor :: Bool -> Bool -> Bool
True `xor` False = True
False `xor` True = True
_     `xor` _    = False

mult :: (Num a) => [a] -> a
mult = foldl' (*) 1

-- ^ adapted from https://rosettacode.org/wiki/Chinese_remainder_theorem#Haskell
chineseRemainder :: [Integer] -> [Integer] -> Maybe Integer
chineseRemainder residues modulii =
  zipWithM modInv crtModulii modulii >>=
  (Just . (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues)
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii

    egcd :: Integer -> Integer -> (Integer, Integer)
    egcd _ 0 = (1, 0)
    egcd a b = (t, s - q * t)
      where
        (s, t) = egcd b r
        (q, r) = a `quotRem` b

    modInv :: Integer -> Integer -> Maybe Integer
    modInv a b =
      case egcd a b of
        (x, y)
          | a * x + b * y == 1 -> Just x
          | otherwise -> Nothing

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (pred n)

manhattan :: (Int, Int) -> Int
manhattan (a, b) = abs a + abs b

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (a, b) (a', b') = abs (a' - a) + abs (b' - b) 

toInt :: Float -> Int
toInt = round

sgn :: Int -> Int
sgn i
  | i < 0 = -1
  | i > 0 = 1
  | otherwise = 0

clamp :: (Int, Int) -> Int -> Int
clamp (lower, upper) n = max lower (min n upper)

-- | TRIPLE FUNCTIONS

fst :: (a, b, c) -> a
fst (v, _, _) = v

snd :: (a, b, c) -> b
snd (_, v, _) = v

thd :: (a, b, c) -> c
thd (_, _, v) = v
  
frequencies :: (Ord a) => [a] -> Map.Map a Int
frequencies as = Map.fromListWith (+) [(a, 1) | a <- as]

composeN :: (a -> a) -> Int -> (a -> a)
composeN f 1 = f
composeN f n = f . composeN f (n - 1)

-- | SEARCH
runBFS :: (Ord a) => (a -> [a]) -> [a] -> [a]
runBFS neighbors roots = bfs neighbors (Seq.fromList roots) Set.empty 

bfs
  :: (Ord a)
  => (a -> [a])
  -> Seq.Seq a
  -> Set.Set a
  -> [a]
bfs neighbors queue seen = case Seq.viewl queue of
  Seq.EmptyL -> []
  (x Seq.:< rest) -> if Set.member x seen
    then bfs neighbors rest seen
    else x:bfs neighbors (rest Seq.>< Seq.fromList (neighbors x)) (Set.insert x seen)
    
-- | https://stackoverflow.com/a/64322395
spfa
    :: (Ord cost , Ord node)
    => ((cost, node) -> [(cost, node)]) -- ^ Where we can go from a node and the cost of that
    -> node                             -- ^ Where we want to get to
    -> (cost, node)                     -- ^ The start position
    -> Maybe (cost, node)              -- ^ Maybe the answer. Maybe it doesn't exist
spfa next target start = search mempty (Set.singleton start)
    where
        search visited toBeVisited = case Set.minView toBeVisited of
            Nothing -> Nothing
            Just ((cost, vertex), withoutVertex)
                | vertex == target            -> Just (cost, vertex)
                | vertex `Set.member` visited -> search visited withoutVertex
                | otherwise                   -> search visitedWithNode withNext
                where
                    visitedWithNode = Set.insert vertex visited
                    withNext = foldr Set.insert withoutVertex $ next (cost, vertex)

runShortestPathsFrom :: (Ord cost, Ord a) => ((cost, a) -> [(cost, a)]) -> (cost, a) -> [(cost, a)]
runShortestPathsFrom neighbors root = shortestPathsFrom neighbors (Seq.singleton root) Set.empty 

shortestPathsFrom
  :: (Ord cost, Ord a)
  => ((cost, a) -> [(cost, a)])
  -> Seq.Seq (cost, a)
  -> Set.Set a
  -> [(cost, a)]
shortestPathsFrom neighbors queue seen = case Seq.viewl queue of
  Seq.EmptyL -> []
  (x Seq.:< rest) -> if Set.member (Prelude.snd x) seen
    then shortestPathsFrom neighbors rest seen
    else x:shortestPathsFrom neighbors (rest Seq.>< Seq.fromList (neighbors x)) (Set.insert (Prelude.snd x) seen)
    
-- | QUEUE
newtype FIFO a = MkFIFO (Seq.Seq a)

singletonFifo :: a -> FIFO a
singletonFifo x = MkFIFO $ Seq.singleton x

pull :: FIFO a -> Maybe (a, FIFO a)
pull (MkFIFO q) = case Seq.viewl q of
  Seq.EmptyL -> Nothing
  (a Seq.:< rest) -> Just (a, (MkFIFO rest))

pushMany :: [a] -> FIFO a -> FIFO a
pushMany as (MkFIFO q) = MkFIFO $ q Seq.>< (Seq.fromList as)