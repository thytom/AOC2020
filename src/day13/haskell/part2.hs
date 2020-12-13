import Data.List.Split (splitOn)
main = do
  f <- readFile("input")
  let (_:buses) = lines f
  let (s:ids) = maketuples $ map convertids $ splitOn "," (buses!!0)
  let start = (0, fst s)
  putStrLn $ show $ timestamp (ids) (start)

-- find lowest multiple of 7 where a mult of 13 comes after n steps
-- make a list, of : that number + lcm(7, 13)*n

-- maketuples :: [Maybe Int] -> [(Int, Int)]
maketuples xs = map (\(Just a, b) -> (a, b)) $ filter (\(a, b) -> a /= Nothing) $ zip xs [0..]

convertids :: String -> Maybe Int
convertids x 
  | x == "x" = Nothing
  | otherwise = Just (read x :: Int)

timestamp :: [(Int, Int)] -> (Int, Int) -> Int
timestamp (x:[]) cur = fst $ consecmul cur x
timestamp (x:xs) cur = timestamp xs $ consecmul cur x

-- Current number, current LCM, target pair
consecmul :: (Int, Int) -> (Int, Int) -> (Int, Int)
consecmul (target, curlcm) (num, offset) = (head [x | x<-[target,target+curlcm..], (x+offset) `mod` num == 0], lcm curlcm num)
