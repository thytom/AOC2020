import Data.List (sort)
main = do
  f <- readFile("input")
  let ls = (sort $ map (read :: String -> Int)$ lines f)
  let lswithmax = wrapBeginEnd ls
  putStrLn $ show $ ans lswithmax
    where ans xs = (countDiff 0 3 xs) * (countDiff 0 1 xs)

wrapBeginEnd xs = (0:xs) ++ [(maximum xs) + 3]

countDiff n i [_] = n
countDiff n i (a:b:xs)
  | a+i == b = countDiff (n+1) i (b:xs)
  | otherwise = countDiff n i (b:xs)
