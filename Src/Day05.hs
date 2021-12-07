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
  | x2 - x1 == y2 - y1 = let lowerX = min x1 x2
                             upperX = max x1 x2
                             lowerY = min y1 y2
                             upperY = max y1 y2
                         in [ Point x y | x <- [lowerX..upperX], y <- [lowerY..upperY], upperX - x == upperY - y ]
  | x2 - x1 == y1 - y2 = let lowerX = min x1 x2
                             upperX = max x1 x2
                             lowerY = min y1 y2
                             upperY = max y1 y2
                         in [ Point x y | x <- [lowerX..upperX], y <- [lowerY..upperY], upperX - x == y - lowerY ]

main :: IO ()
main = do
  inputStr <- readFile "input/day05"
  let input = lines inputStr
  -- print input
  let inputVectors = map (parsePoint . map (splitOn ",") . splitOn " -> ") input
  print inputVectors

  let vectorsParallelToCoordinateGrid = filter isParallelToCoordinateGrid inputVectors
  print vectorsParallelToCoordinateGrid

  -- determine necessary size
  let sideLength = maximum $ map parseInt $ concatMap (concatMap (splitOn ",") . splitOn " -> ") input
  print $ "sideLength: " ++ show sideLength

  let toIndex :: Point -> Int
      toIndex (Point x y) = x * sideLength + y

  let intersections = group $ sort $ map toIndex $ concatMap toLine vectorsParallelToCoordinateGrid
  let part1 = length $ filter (\it -> length it > 1) intersections
  putStrLn "part1" >> print part1

  -- let print2d = mapM_ print
  -- print2d $ map toLine inputVectors
  let intersections = group $ sort $ map toIndex $ concatMap toLine inputVectors
  let part2 = length $ filter (\it -> length it > 1) intersections
  putStrLn "part2" >> print part2
