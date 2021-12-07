module Main where

import Src.Util
import Data.List
import Data.Char

mostOften = foldl1 (\acc x -> if snd acc > snd x then acc else x)
binToDec l = sum $ map (2^) $ elemIndices 1 $ reverse l
binInvert = map $ (`mod` 2) . (1+)

main :: IO ()
main = do
  inputStr <- readFile "input/day03"
  let input = splitOn '\n' inputStr
  -- print input

  let bitLists = map (map digitToInt) $ transpose input
  let gammaBits = map (fst . mostOften . map (\l@(x:xs) -> (x, length l)) . group . sort) bitLists
  let epsilonBits = binInvert gammaBits
  let gamma = binToDec gammaBits
  let epsilon = binToDec epsilonBits

  print $ "gamma: " ++ show gamma
  print $ "epsilon: " ++ show epsilon

  let part1 = gamma * epsilon
  print "part1:"
  print part1
