module Main where

import Data.List.Split (splitOn)
import Data.Ratio
import Data.Bifunctor (bimap)
import Data.List (tails)

import Advent (challenge)
import Utils (readInt)

type HailStone = ((Integer, Integer, Integer), (Integer, Integer, Integer))
type Challenge = [HailStone]

parse :: String -> Challenge
parse = map (p . map i . concatMap (splitOn ", ") . splitOn " @ ") . lines
  where p [px, py, pz, vx, vy, vz] = ((px, py, pz), (vx, vy, vz))
        i = toInteger . readInt

solveSystem :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Maybe (Rational, Rational)
solveSystem a b e c d f
  | det == 0 || t1 < 0 || t2 < 0 = Nothing
  | otherwise = Just (t1, t2)
    where det = a*d - b*c
          t1 = (e*d - b*f) % det
          t2 = (a*f - e*c) % det

intersection :: HailStone -> HailStone -> Maybe (Integer, Integer)
intersection ((x1, y1, _), (vx1, vy1, _)) ((x2, y2, _), (vx2, vy2, _)) =
  let dx = x2 - x1
      dy = y2 - y1
      t = solveSystem vx1 (-vx2) dx vy1 (-vy2) dy
  in case t of
      Nothing -> Nothing
      Just (t1, t2) ->
          let intersectionPoint1 =  ( (fromIntegral x1) + (t1 * fromIntegral vx1)
                                    , (fromIntegral y1) + (t1 * fromIntegral vy1)
                                    )
              intersectionPoint2 = ( (fromIntegral x2) + (t2 * fromIntegral vx2)
                                   , (fromIntegral y2) + (t2 * fromIntegral vy2)
                                   )
            in if intersectionPoint1 == intersectionPoint2
               then Just $ bimap round round intersectionPoint1
               else Nothing

intersections :: [HailStone] -> [(Integer, Integer)]
intersections stones = [ i
                       | (s1, rest) <- zip stones (tails (tail stones))
                       , s2 <- rest
                       , Just i <- [intersection s1 s2]
                       ]

inTestArea :: Integer -> Integer -> (Integer, Integer) -> Bool
inTestArea lo hi (x, y) = lo <= x && x <= hi && lo <= y && y <= hi

part1 :: Challenge -> String
part1 = show . length . filter (inTestArea 200000000000000 400000000000000) . intersections

part2 :: Challenge -> String
part2 = const "see day24.py (I'll have to figure out how to use z3 with haskell sometime)"

main :: IO ()
main = challenge "24" parse part1 part2