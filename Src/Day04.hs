module Main where

import Src.Util (splitOn)
import Text.Printf
import Data.List
import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust, isNothing)
import Debug.Trace

print_ x =  putStr $ show x ++ "\t" 

printBoards = mapM_ printBoard 
  where printBoard xs = mapM_ printRow xs >> putStrLn ""
        printRow xs = mapM_ print_ xs >> putStrLn ""

parseInt = read :: String -> Int

bingo draws = findIndex (boardRowIsFullyDrawn draws)

boardRowIsFullyDrawn draws row = (==0) $ length $ Set.difference row draws

firstBingo (n:ns) draws boards = do
  let drawSet = Set.fromList (take n draws)
  let result = map (bingo drawSet) boards
  let checkResult [] = False
      checkResult (r:rs) = isJust r || checkResult rs
  trace (
    "draws" ++ show (take n draws) ++
    "\nresult" ++ show result
    ) $ if checkResult result then (result, take n draws) else firstBingo ns draws boards

lastBingoDraw (n:ns) draws boards = do
  let drawSet = Set.fromList (take n draws)
  let result = map (bingo drawSet) boards
  if all isJust result then n else lastBingoDraw ns draws boards

main :: IO ()
main = do
  inputStr <- readFile "input/day04"
  let input = lines inputStr
  let inputParts = splitOn "" input
  -- print inputParts
  let draws = map parseInt $ splitOn ',' $ head $ head inputParts
  print $ "draws: " ++ show draws

  let boards = map (map (map parseInt . filter (/= "") . splitOn ' ')) (tail inputParts)
  print $ "boards: " ++ show boards
  let boardSets = map (map Set.fromList . (\ x -> x ++ transpose x)) boards

  putStrLn "boards" >> printBoards boardSets

  let bingoResult = firstBingo [1..] draws boardSets
  let bingoIndex = (\x -> (find isJust x, findIndex isJust x)) $ fst bingoResult
  let winner = boards !! fromJust (snd bingoIndex)
  let finalDraws = snd bingoResult
  let winnerUndrawnSum = sum $ Set.toList $ Set.difference (Set.fromList $ concat winner) (Set.fromList finalDraws)
  let part1 = winnerUndrawnSum * last finalDraws
  putStrLn "winner" >> print winner
  putStrLn "bingoIndex" >> print bingoIndex
  putStrLn "finalDraws" >> print finalDraws
  putStrLn "winnerUndrawnSum" >> print winnerUndrawnSum
  putStrLn "last draw" >> print (last finalDraws)
  putStrLn ""
  putStrLn "part1" >> print part1
  putStrLn ""

  let lastBingoDrawIndex = lastBingoDraw [1..] draws boardSets
  let finalLastDraws = take lastBingoDrawIndex draws
  let previousBingo = map (bingo (Set.fromList (take (lastBingoDrawIndex - 1) draws))) boardSets
  let lastBingoIndex = fromJust $ findIndex isNothing previousBingo
  let lastWinner = boards !! lastBingoIndex
  let lastWinnerUndrawnSum = sum $ Set.toList $ Set.difference (Set.fromList $ concat lastWinner) (Set.fromList finalLastDraws)
  let part2 = lastWinnerUndrawnSum * last finalLastDraws
  print finalLastDraws
  putStrLn "previousBingo" >> print previousBingo
  putStrLn "lastWinner" >> print lastWinner
  putStrLn "part2" >> print part2