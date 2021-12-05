module Main where

import Data.List.Split

zipWithNext :: Ord a => [a] -> [(a, Maybe a)]
zipWithNext [] = []
zipWithNext [x] = [(x, Nothing)]
zipWithNext (x:y:rest) = (x, Just y) : zipWithNext (y : rest)

increased :: Ord a => (a, Maybe a) -> Bool
increased (_, Nothing) = False
increased (x, Just y) = x < y

zipAsSlidingWindow :: (Ord a, Num a) => [a] -> [(a, Maybe a, Maybe a, Maybe a)]
zipAsSlidingWindow [] = []
zipAsSlidingWindow (x:y:z:n:rest) = (x, Just y, Just z, Just n) : zipAsSlidingWindow (y:z:n:rest)
zipAsSlidingWindow (x:y:z:xs) = [(x, Just y, Just z, Nothing)]
zipAsSlidingWindow (x:y:xs) = [(x, Just y, Nothing, Nothing)]
zipAsSlidingWindow (x:xs) = [(x, Nothing, Nothing, Nothing)]

increasedThree :: (Num a, Ord a) => (a, Maybe a, Maybe a, Maybe a) -> Bool
increasedThree (_, Nothing, _, _) = False
increasedThree (x, Just y, Nothing, _) = False
increasedThree (x, Just y, Just z, Nothing) = False
increasedThree (x, Just y, Just z, Just n) = (x + y + z) < (y + z + n)

main :: IO ()
main = do
  input <- readFile "input/day01"
  let nums = [read x :: Int | x <- splitOn "\n" input]
  let zipped = zipWithNext nums
  -- print zipped
  let part1 = length $ filter increased zipped
  print part1

  let zippedAsSliding = zipAsSlidingWindow nums
  -- print zippedAsSliding
  let part2 = length $ filter increasedThree zippedAsSliding
  print part2
