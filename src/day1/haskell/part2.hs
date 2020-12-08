main = do 
  input <- readFile("input")
  let ints = [read x :: Int | x<-lines input]
  putStrLn $ show $ head [x * y * (2020-x-y) | x<-ints, y<-ints, (2020-x-y) `elem` ints]
