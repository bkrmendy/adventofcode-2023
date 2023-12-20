module Main where

import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict ((!))

import qualified Data.Sequence as Seq
import Data.Sequence (ViewL(EmptyL, (:<)), (><))

import Data.List.Split (splitOn)
import Data.List (foldl')

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

import Utils (takeUntil)
import Advent (challenge)

data ModuleType = FlipFlop Bool
                | Conjunction (M.HashMap String Int)
                | Broadcaster
                deriving Show

type Outputs = M.HashMap String [String]
type ModuleState = M.HashMap String ModuleType
type Cables = (Outputs, ModuleState)

type Challenge = Cables

parse :: String -> Challenge
parse input = (outputs, moduleStates)
  where
    ls = [(f, o) | line <- lines input, let [f, t] = splitOn " -> " line, let o = splitOn ", " t]
    
    raw = M.fromList $ do
      (f, out) <- ls
      case f of
        "broadcaster" -> return (f, ('b', out))
        ('%':name) -> return (name, ('%', out))
        ('&':name) -> return (name, ('&', out))
        
    outputs = M.fromList $ do
      (m, (_, out)) <- M.toList raw
      return (m, out)
       
    updateModuleState :: ModuleType -> ModuleType -> ModuleType
    updateModuleState (Conjunction xs) (Conjunction ys) = Conjunction (M.union xs ys)
    updateModuleState _ new = new
           
    moduleStates = M.fromListWith updateModuleState $ do
      (key, (tag, _)) <- M.toList raw
      case tag of
        'b' -> return (key, Broadcaster)
        '%' -> return (key, FlipFlop False)
        '&' -> do
          (k, (_, oo)) <- M.toList raw
          guard $ key `elem` oo
          return (key, Conjunction (M.singleton k 0)) 

send :: Outputs -> String -> Int -> [(String, String, Int)]
send c from signal = [(from, dst, signal) | dst <- c ! from]

type PulseMonad = ReaderT Outputs (StateT ModuleState Identity)

update :: String -> String -> Int -> PulseMonad [(String, String, Int)]
update src recipient signal = do
  moduleState <- gets $ M.lookup recipient
  case moduleState of
    Nothing -> return []
    Just Broadcaster -> error "message sent to broadcaster module"
    Just (FlipFlop on) ->
      if signal == 1
      then return []
      else do
        outputs <- ask
        modify' $ M.insert recipient (FlipFlop (not on))
        return $ send outputs recipient (if on then 0 else 1)
  
    Just (Conjunction memory) ->
      let
        updatedMem = M.insert src signal memory
        signalToSend = if all (== 1) (M.elems updatedMem) then 0 else 1
      in do
        outputs <- ask
        modify' $ M.insert recipient (Conjunction updatedMem)
        return $ send outputs recipient signalToSend

pulse :: Seq.Seq (String, String, Int) -> PulseMonad [(String, String, Int)]
pulse queue = case Seq.viewl queue of
  EmptyL -> return []
  (src, dst, signal) :< rest -> do
     step <- update src dst signal
     next <- pulse (rest >< (Seq.fromList step))
     return $ (src, dst, signal):next

count :: (Int, Int) -> Int -> (Int, Int)
count (hi, lo) 0 = (hi, lo + 1)
count (hi, lo) 1 = (hi + 1, lo)

push :: Int -> (Int, Int) -> PulseMonad (Int, Int)
push 0 acc = return acc
push times (lo, hi) = do
  start <- asks $ \output -> Seq.fromList $ send output "broadcaster" 0
  es <- pulse start
  let (nh, nl) = foldl' (\a (_, _, s) -> count a s) (lo, hi) es 
  push (times - 1) (nh, nl + 1)
   
spin :: PulseMonad [[(String, String, Int)]]
spin = do
  start <- asks $ \output -> Seq.fromList $ send output "broadcaster" 0
  es <- pulse start
  next <- spin
  return (es:next)
  

runPulse :: PulseMonad a -> Cables -> (a, Cables)
runPulse f (output, states) = (result, (output, nextStates))
  where (result, nextStates) = runIdentity $ runStateT (runReaderT f output) states

part1 :: Challenge -> String
part1 = show . uncurry (*) . fst . runPulse (push 1000 (0, 0))

part2 :: Challenge -> String
part2 cables = show $ product $ map c inputs
  where
    Conjunction out = (snd cables) ! "gf"
    inputs = M.keys $ out
    sent s = any (\(src, dst, signal) -> src == s && dst == "gf" && signal == 1)
    c s = succ $ length $ takeUntil (sent s) $ fst $ runPulse spin cables

main :: IO ()
main = challenge "20" parse part1 part2