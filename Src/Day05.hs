module Main where

import Data.List
import Data.List.Split (splitOn)
import Data.Text (pack, unpack,replace)

{- 
  create list of functions that take another line as input and check for intersections
-}

parseInt = read :: String -> Int

data Point = Point Int Int deriving (Show)
point0 = Point 0 0

data Vector = Vector Point Point deriving (Show)


sub :: Vector -> Vector -> Vector
sub (Vector (Point x1 y1) (Point x2 y2)) (Vector (Point a1 b1) (Point a2 b2)) = Vector (Point (x1 - a1) (y1 - b1)) (Point (x1 - a2) (y2 - b2))

subP :: Point -> Point -> Point
subP (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

dot :: Vector -> Vector -> Int
dot (Vector (Point x1 y1) (Point x2 y2)) (Vector (Point a1 b1) (Point a2 b2)) = (x2 - x1) * (a2 - a1) + (y2 - y1) * (b2 - b1)

parsePoint :: [[String]] -> Vector
parsePoint ((x1:y1:_):(x2:y2:_):_) = Vector (Point (parseInt x1) (parseInt y1)) (Point (parseInt x2) (parseInt y2))

intersects :: Vector -> Vector -> Bool
intersects (Vector p1 p2) (Vector p3 p4) = do
   let d1 = Vector p1 p3 `dot` Vector p4 p3
   let d2 = Vector p2 p3 `dot` Vector p4 p3
   let d3 = Vector p3 p1 `dot` Vector p2 p1
   let d4 = Vector p4 p1 `dot` Vector p2 p1
   (d1 < 0 && d2 > 0) || (d1 > 0 && d2 < 0) && (d3 > 0 && d4 < 0) || (d3 < 0 && d4 > 0)

isParallelToCoordinateGrid (Vector (Point x1 y1) (Point x2 y2))
  | x1 == x2 || y1 == y2 = True
  | otherwise = False

countOfIntersections [] _ = 0
countOfIntersections (x:xs) all = length (filter (intersects x) all) + countOfIntersections xs all

-- 0,9 -> 5,9
-- [0,9 1,9 2,9 3,9 4,9 5,9]

toLine :: Vector -> [Point]
toLine (Vector (Point x1 y1) (Point x2 y2))
  | x1 == x2 = let lower = min y1 y2
                   upper = max y1 y2
               in [ Point x1 y | y <- [lower..upper] ]
  | y1 == y2 = let lower = min x1 x2
                   upper = max x1 x2
               in [ Point x y1 | x <- [lower..upper] ]

addLine :: [Vector] -> [[Int]] -> [[Int]]
addLine [] arr = arr
addLine (v:vs) arr = do
  let line = toLine v
  let addPoint :: [Point] -> [[Int]] -> [[Int]]
      addPoint [] arr = arr
      addPoint (p:ps) arr = addPoint ps [[addConditionally p arr row col | col <- [0..(length (head arr) - 1)]] | row <- [0..(length arr - 1)]]
        where addConditionally (Point x y) arr row col = if x == row && y == col then (arr !! row !! col) + 1 else arr !! row !! col
  addLine vs $ addPoint line arr
  -- addLine vs $ addPoint line arr

main :: IO ()
main = do
  inputStr <- readFile "input/day05"
  let input = lines inputStr
  -- print input
  let inputVectors = map (parsePoint . map (splitOn ",") . splitOn " -> ") input
  -- print inputVectors


  let vectorsParallelToCoordinateGrid = filter isParallelToCoordinateGrid inputVectors
  -- print vectorsParallelToCoordinateGrid
  -- print $ countOfIntersections vectorsParallelToCoordinateGrid vectorsParallelToCoordinateGrid

  -- hack: determine necessary size
  -- print $ last $ sort $ map parseInt $ concat $ map (concat . map (splitOn ",") . splitOn " -> ") input
  let resultGrid = addLine vectorsParallelToCoordinateGrid [[0 | y <- [0..20]] | x <- [0..20]]
  -- let print2d = mapM_ print
  -- print2d resultGrid
  -- print $ take 3 vectorsParallelToCoordinateGrid
  -- print $ map toLine (take 3 vectorsParallelToCoordinateGrid)
  let count2OrHigher = length $ concat $ filter (not . null) $ map (filter (>=2)) $ group $ sort (concat resultGrid)
  let part1 = count2OrHigher
  putStrLn "part1" >> print part1
