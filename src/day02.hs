module Main where

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

applyIfNotEmpty _ [] = []
applyIfNotEmpty f xs = f xs

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn del xs = do
  let matches = span (/= del) xs
  let rest = safeTail $ snd matches
  fst matches : applyIfNotEmpty (splitOn del) rest

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

calculate :: Direction Int -> [Int]
calculate (Forward a) = [0, a]
calculate (Up a) = [-a, 0]
calculate (Down a) = [a, 0]


main :: IO ()
main = do
  input <- readFile "input/day02"
  let nums = splitOn '\n' input
  print nums

  let dirs = map parseDir nums
  print dirs

  let evaluated = foldl (\acc dir -> zipWith (+) acc (calculate dir)) [0,0] dirs
  let depth = head evaluated
  let horPos = evaluated !! 1
  print $ "depth: " ++ show depth
  print $ "horizontal position: " ++ show horPos
  let part1 = depth * horPos
  print part1
