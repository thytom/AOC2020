main = do
  input <- readFile("input")
  let li = map (read :: String -> Int) $ lines input
  let xs = splitAsGroups 26 li
  let firstinvalid = head [last x | x <- xs, checkValid x == False]
  putStrLn $ show $ (\x -> (maximum x) + (minimum x)) $ anyConsecutiveSum firstinvalid li

checkValid i = any (==True) [(last i) == (a + b) | a<-l, b<-l, a /= b]
  where l = init i

splitAsGroups _ (x:[]) = []
splitAsGroups n x = (take n x) : splitAsGroups n (drop 1 x)

anyConsecutiveSum :: Int -> [Int] -> [Int]
anyConsecutiveSum n xs = head [subset f l xs
  | f<-[0..(length xs)-1], l<-[f..(length xs)-1], sum (subset f l xs) == n]

subset :: Int -> Int -> [a] -> [a]
subset f l xs = drop f . take (l+1) $ xs
