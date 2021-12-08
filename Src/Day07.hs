module Main where

import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

parseInt = read :: String -> Int

distanceToFuel x = sum $ take x [1..]

consumeFuel :: Int -> Int -> Int
consumeFuel start end = distanceToFuel $ abs (start - end)

calculateFuelConsumption range input f = [sum [f x t | x <- input ] | t <- range]

main :: IO ()
main = do
  inputStr <- readFile "input/day07"
  let input = map parseInt $ splitOn "," inputStr
  -- print input

  let range = [minimum input..maximum input]
  let results1 = calculateFuelConsumption range input (\x t -> abs (x - t))
  -- print $ fromJust $ elemIndex (minimum results1) results1
  let part1 = minimum results1
  putStrLn "part1" >> print part1

  let results2 = calculateFuelConsumption range input consumeFuel
  -- print $ fromJust $ elemIndex (minimum results2) results2
  let part2 = minimum results2
  putStrLn "part2" >> print part2

