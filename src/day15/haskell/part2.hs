import Data.List
import Data.IntMap as M

initial = [  6, 13, 1, 15, 2, 0 ]

test = [ 0 , 3 , 6 ]

main = do
  let start = (\x -> (length x, M.fromList (zip (init x) [1..]), last x)) initial
  putStrLn . show $ (\(_, _, x)->x) $ nextFor 30000000 $ next start

nextFor :: Int -> (Int, M.IntMap Int, Int) -> (Int, M.IntMap Int, Int)
nextFor n xs = until (\(i,_,_) -> i == n) next xs

next :: (Int, M.IntMap Int, Int) -> (Int, M.IntMap Int, Int)
next (i, m, n) = (i+1, M.insert n i m, o)
  where o = case m M.!? n of
              Nothing -> 0
              Just x -> i - x
