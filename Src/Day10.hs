module Main where

import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)


op = "[{(<"
cl = "]})>"

expect c = fromJust $ lookup c $ zip op cl

check1 = check1' 0 ""
check1' i s [] = (0, "")
check1' i s (c:cs)
    | c `elem` op = check1' (i+1) (expect c:s) cs
    | c `elem` cl && (not.null) s && c == head s = check1' (i+1) (tail s) cs
    | otherwise = (i, c : s)

check2 = check2' 0 ""
check2' i s [] = (0, s)
check2' i s (c:cs)
    | c `elem` op = check2' (i+1) (expect c:s) cs
    | c `elem` cl && (not.null) s && c == head s = check2' (i+1) (tail s) cs
    | otherwise = (i, "")

value1 ')' = 3
value1 ']' = 57
value1 '}' = 1197
value1 '>' = 25137

value2 = foldl (\s c -> s * 5 + value2' c)
value2' ')' = 1
value2' ']' = 2
value2' '}' = 3
value2' '>' = 4

middle [x] = x
middle xs = middle $ (init . tail) xs

main :: IO ()
main = do
  inputStr <- readFile "input/day10"
  let inputLines = lines inputStr
  let input = inputLines

  let corrupteds = map head $ filter (not.null) $ map (snd . check1) input
  let part1 = sum $ map value1 corrupteds
  putStrLn "part1" >> print part1

  let incompletes = filter (not.null) $ map (snd . check2) input
  let part2 = middle $ sort $ map (value2 0) incompletes
  putStrLn "part2" >> print part2
