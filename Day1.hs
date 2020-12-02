module Day1 where

import Data.List as List
import Data.IntSet as Set (fromList, intersection, foldr)

runDay1 :: IO ()
runDay1 = do
  x <- readFile "input"
  let ls = lines x
  let numbers = map read ls
  print $ solve 2 numbers
  print $ solve 3 numbers

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = return []
combinations n xs = do y:xs' <- tails xs
                       ys <- combinations (n-1) xs'
                       return (y:ys)

solve :: Int -> [Int] -> Int
solve k ns =
  let
    combs = combinations (k - 1) ns
  in
    Set.foldr (*) 1 ((fromList ns) `intersection` (fromList $ map ((2020 -) . sum) combs))
