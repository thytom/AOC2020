import Data.List (sort, nub, subsequences)
main = do
  f <- readFile("input")
  let ls = 0:(sort $ map (read :: String -> Int)$ lines f)
  putStrLn $ show $ product $ map (getWays . length) $ filter (\x -> (length x) > 2) $ splitConsecutive ls [] []

splitConsecutive (x:[]) ys b = (x:b):ys
splitConsecutive (x:y:xs) ys b
  | x+1 == y = splitConsecutive (y:xs) ys (x:b)
  | otherwise = splitConsecutive (y:xs) ((x:b):ys) []

getWays n 
  | n == 3 = 2
  | n == 4 = 4
  | n == 5 = 7
  | otherwise = 1
