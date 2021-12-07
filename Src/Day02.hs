module Main where

import Src.Util

data Direction a = 
  Forward a |
  Down a |
  Up a
  deriving (Show)

parseDir :: [Char] -> Direction Int
parseDir xs = do
  let split = splitOn ' ' xs
  let lastSplit = read (last split) :: Int
  case head split of "forward" -> Forward lastSplit
                     "down"    -> Down lastSplit
                     "up"      -> Up lastSplit

-- aim, depth, horPos
calculate :: [Int] -> Direction Int -> [Int]
calculate (x:y:z:_) (Forward a) = [x, y + x * a, z + a]
calculate (x:y:z:_) (Up a) = [x - a, y, z]
calculate (x:y:z:_) (Down a) = [x + a, y, z]


main :: IO ()
main = do
  input <- readFile "input/day02"
  let nums = splitOn '\n' input
  print nums

  let dirs = map parseDir nums
  print dirs

  let evaluated = foldl calculate [0,0,0] dirs
  let depth = evaluated !! 1
  let horPos = evaluated !! 2
  print $ "depth: " ++ show depth
  print $ "horizontal position: " ++ show horPos
  let part2 = depth * horPos
  print part2
