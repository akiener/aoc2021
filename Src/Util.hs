module Src.Util
( splitOn
) where 
    
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
