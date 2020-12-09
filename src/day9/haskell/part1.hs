main = do
  input <- readFile("input")
  let li = splitAsGroups 26 $ map (read :: String -> Int) $ lines input
  putStrLn $ show $ head [last x | x <- li, checkValid x == False]

checkValid i = any (==True) [(last i) == (a + b) | a<-l, b<-l, a /= b]
  where l = init i

splitAsGroups _ (x:[]) = []
splitAsGroups n x = (take n x) : splitAsGroups n (drop 1 x)
