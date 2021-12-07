module Main where

import Src.Util
import Data.List
import Data.Char
import Debug.Trace

foldBySnd order = foldl1 (\acc x -> if snd acc `order` snd x then acc else x)
maxBySnd = foldBySnd (>)
minBySnd = foldBySnd (<)
binToDec l = sum $ map (2^) $ elemIndices 1 $ reverse l
binInvert = map $ (`mod` 2) . (1+)

findCommonBit f = map (map (\l@(x:xs) -> (x, length l)) . f . group . sort)
findMostCommonBit = findCommonBit id
findLeastCommonBit = findCommonBit reverse

findGeneric _ _ _ _ [a] = a
findGeneric fBy fFind (n:ns) prefix input = do
  let matches = filter (isPrefixOf (take n prefix)) input
  let recurse [a] = a
      recurse xs = do
        let searchedBit = fst $ fBy $ (!! n) $ fFind $ transpose matches
        let newPrefix = prefix ++ [searchedBit]
        -- trace (
        --   "r matches: " ++ show matches ++
        --   "\n\tt matches: " ++ show ((!! n) $ fFind $ transpose matches) ++
        --   "\n\tsearchedBit: " ++ show searchedBit ++
        --   "\n\tnewPrefix: " ++ show newPrefix
        -- ) 
        findGeneric fBy fFind ns newPrefix matches
  recurse matches

findOxygen = findGeneric maxBySnd findMostCommonBit
findCO2 = findGeneric minBySnd findLeastCommonBit

main :: IO ()
main = do
  inputStr <- readFile "input/day03"
  let input = lines inputStr
  print input
  print $ map (map digitToInt) input

  let bitLists = map (map digitToInt) input
  let bitGroups = findMostCommonBit $ transpose bitLists
  let gammaBits = map (fst . maxBySnd) bitGroups
  let epsilonBits = binInvert gammaBits
  let gamma = binToDec gammaBits
  let epsilon = binToDec epsilonBits

  print $ "gamma: " ++ show gamma
  print $ "epsilon: " ++ show epsilon

  let part1 = gamma * epsilon
  print "part1:"
  print part1

  let oxygen = binToDec $ findOxygen [1..] (take 1 gammaBits) bitLists
  let co2 = binToDec $ findCO2 [1..] (take 1 epsilonBits) bitLists
  print $ "oxygen: " ++ show oxygen
  print $ "co2: " ++ show co2
  print "part2:"
  let part2 = oxygen * co2
  print part2
