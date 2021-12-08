module Main where

import Data.List.Split (splitOn)

parseInt = read :: String -> Int

fish2 :: Int -> [Int] -> [Int]
fish2 n state = last $ take (n+1) $ iterate live2 state

live2 xs = [xs !! 1, xs !! 2, xs !! 3, xs !! 4, xs !! 5, xs !! 6, (xs !! 7) + head xs, xs !! 8, head xs]

toState state = [length $ filter (==n) state | n <- [0..8]]

main :: IO ()
main = do
  inputStr <- readFile "input/day06"
  let input = map parseInt $ splitOn "," inputStr
  let part2 = sum $ fish2 256 (toState input)
  putStrLn "part2" >> print part2
