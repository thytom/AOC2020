import Data.List.Split (splitOn)
main = do
  f <- readFile("input")
  let (timestamp:buses) = lines f
  let ts = read timestamp :: Int
  let ids = map (read :: String -> Int) $ filter (/="x") $ splitOn "," (buses!!0)
  putStrLn $ show $ (\(x, y)-> (x-ts) * y) $ best ids ts

closest :: Int -> Int -> Int
closest x y = head [x*n | n<-[1..], (x*n) >= y]

best :: [Int] -> Int -> (Int, Int)
best xs y = foldl (pairmin) (maxBound :: Int, 0) [(closest x y, x) | x<-xs]

pairmin (a, x) (b, y)
  | a < b = (a, x)
  | b < a = (b, y)
  | otherwise = (a, x)
