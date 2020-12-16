import Data.List
import Data.Maybe (fromJust)

main = do
  let start = reverse [6,13,1,15,2,0]
  putStrLn $ show.head $ nextFor (2020 - (length start)) start

nextFor n xs 
  | n <= 0 = xs
  | otherwise = nextFor (n-1) (next xs)

next (x:xs) 
  | previousIndex == Nothing = 0:x:xs
  | otherwise = ((length xs) - ((length xs) - ((fromJust previousIndex) + 1))):x:xs
  where previousIndex = elemIndex x xs
