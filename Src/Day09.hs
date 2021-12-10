module Main where

import Data.List
import Data.List.Split (splitOn)
import Debug.Trace

parseInt = read :: String -> Int

getFrom x y xs = xs !! y !! x

getXDim xs = length (head xs) - 1
getYDim xs = length xs - 1

isLowest :: Int -> Int -> [[Int]] -> Bool
isLowest x y xs
  | y == 0 && x == 0 = height < get x (y + 1) && height < get (x + 1) y
  | y == 0 && x == xDim = height < get x (y + 1) && height < get (x - 1) y
  | x == 0 && y == yDim = height < get (x + 1) y && height < get x (y - 1)
  | x == xDim && y == yDim = height < get (x - 1) y && height < get x (y - 1)
  | x == 0 = height < get (x + 1) y && height < get x (y - 1) && height < get x (y + 1)
  | y == 0 = height < get (x - 1) y && height < get (x + 1) y && height < get x (y + 1)
  | x == xDim = height < get (x - 1) y && height < get x (y - 1) && height < get x (y + 1)
  | y == yDim = height < get (x - 1) y && height < get (x + 1) y && height < get x (y - 1)
  | otherwise = height < get (x - 1) y && height < get (x + 1) y && height < get x (y - 1) && height < get x (y + 1)
  where get x y = getFrom x y xs
        height = get x y
        xDim = getXDim xs
        yDim = getYDim xs

recurseBasin [] []
recurseBasin x y basinNodes

isAdjacentTo x y (x1,y1)
  | x == x1 = abs (y - y1) == 1
  | y == y1 = abs (x - x1) == 1
  | otherwise = False

main :: IO ()
main = do
  inputStr <- readFile "input/day09ex"
  let inputLines = lines inputStr
  let input = map (map (parseInt . (:[]))) inputLines
  -- print input
  let print2d = mapM_ print
  print2d input

  let xDim = getXDim input
      yDim = getYDim input

  let riskLevels = map (+1) $ [getFrom x y input | x <- [0..xDim], y <- [0..yDim], isLowest x y input]
  let part1 = sum riskLevels
  putStrLn "part1" >> print part1

  let basinRoots = [(x, y) | x <- [0..xDim], y <- [0..yDim], isLowest x y input]
  print $ basinRoots
