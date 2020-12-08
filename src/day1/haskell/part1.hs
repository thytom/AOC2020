main = do 
  input <- readFile("input")
  let ints = [read x :: Int | x<-lines input]
  putStrLn $ show $ head [x * (2020-x) | x<-ints, (2020-x) `elem` ints]
