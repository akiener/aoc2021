module Main where

import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust)

parseInt = read :: String -> Int

filterNot pred = filter $ not . pred

lengthToDigit = M.fromList [(2,1), (4,4), (3,7), (7,8)]
digitToLength = M.fromList [(v,k) | (k,v) <- M.toList lengthToDigit]
byLength d inputs = fromJust $ find (\x -> length x == fromJust (M.lookup d digitToLength)) inputs

nine s4 s7 sX
  | not (null ((s4 `union` s7) \\ sX)) = False
  | length (sX \\ (s4 `union` s7)) == 1 = True
  | otherwise = False

two s9 s8 sX = s8 \\ s9 == sX \\ s9 && length sX == 5

five s9 s1 sX = length (sX \\ (s9 \\ s1)) == 1

zero s1 sX = length (sX \\ s1) == 4

processLine input output = do
  let s1 = byLength 1 input
  let s4 = byLength 4 input
  let s7 = byLength 7 input
  let s8 = byLength 8 input
  let currentInput1 = filterNot (`elem` [s1,s4,s7,s8]) input

  let s9 = fromJust $ find (nine s4 s7) currentInput1
  let currentInput2 = filter (/= s9) currentInput1

  let s2 = fromJust $ find (two s9 s8) currentInput2
  let currentInput3 = filter (/= s2) currentInput2

  let s5 = fromJust $ find (five s9 s1) currentInput3
  let currentInput4 = filter (/= s5) currentInput3

  let s3 = fromJust $ find (\x -> length x == 5) currentInput4
  let currentInput5 = filter (/= s3) currentInput4

  let s0 = fromJust $ find (zero s1) currentInput5
  let currentInput6 = filter (/= s0) currentInput5

  let s6 = head currentInput6

  -- print $ "1: " ++ s1 ++ ", 4: " ++ s4 ++ ", 7: " ++ s7 ++ ", 8: " ++ s8 ++ ", rest: " ++ show currentInput1
  -- print $ "9: " ++ s9 ++ ", rest: " ++ show currentInput2
  -- print $ "2: " ++ s2 ++ ", rest: " ++ show currentInput3
  -- print $ "5: " ++ s5 ++ ", rest: " ++ show currentInput4
  -- print $ "3: " ++ s3 ++ ", rest: " ++ show currentInput5
  -- print $ "0: " ++ s0 ++ ", rest: " ++ show currentInput6
  -- print $ "6: " ++ s6

  let digitStrings = map sort [s0, s1, s2, s3, s4, s5, s6, s7, s8, s9]
  -- print digitStrings
  -- print $ map sort $ head outputs
  -- print $ map (fromJust . (`elemIndex` digitStrings) . sort) (head outputs)
  let resultDigits = map (fromJust . (`elemIndex` digitStrings) . sort) output
  let result = sum $ zipWith (\ n x -> x * 10 ^ n) [3, 2 .. 0] resultDigits
  result

main :: IO ()
main = do
  inputStr <- readFile "input/day08"
  let inputLines = lines inputStr
  let inputStrParts = map (splitOn "|") inputLines
  let inputs = map (filter (/="") . splitOn " " . head) inputStrParts
  let outputs = map (filter (/="") . splitOn " " . last) inputStrParts

  let part1 = length $ filter (`elem` digitToLength) (concatMap (map length) outputs)
  putStrLn "part1" >> print part1

  let results = zipWith processLine inputs outputs
  let part2 = sum results
  putStrLn "part2" >> print part2