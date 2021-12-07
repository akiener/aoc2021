module Main where

import Data.List
import Data.List.Split (splitOn)
import Debug.Trace

parseInt = read :: String -> Int

{-
  new lanternfish every 7 days
-}

fish :: Int -> [Int] -> [Int]
fish 0 state = state
fish n state = do
  let update = live state
  let newFish = length $ filter (==0) state
  trace (show n) $ fish (n-1) (update ++ replicate newFish 8)

live :: [Int] -> [Int]
live [] = []
live (0:xs) = 6 : live xs
live (x:xs) = (x-1) : live xs

fish2 :: Int -> [Int] -> [Int]
fish2 n state = last $ take (n+1) $ iterate live2 state

firstEigth = [0..8]

live2 xs = [xs !! 1, xs !! 2, xs !! 3, xs !! 4, xs !! 5, xs !! 6, (xs !! 7) + head xs, xs !! 8, head xs]

toState state = [ length $ filter (==n) state | n <- firstEigth]

main :: IO ()
main = do
  inputStr <- readFile "input/day06"
  let input = map parseInt $ splitOn "," inputStr
  print input
  print $ toState input

  -- let part1 = length $ fish 1 input
  -- print part1
  -- putStrLn "part1" >> print part1

  let part2 = sum $ fish2 256 (toState input)
  putStrLn "part2" >> print part2