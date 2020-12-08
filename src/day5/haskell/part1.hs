main :: IO ()
main = do
  input <- readFile "input"
  let ids = map seatid $ lines input
  putStrLn $ show $ maximum ids

seatid :: String -> Int
seatid s 
  | length s == 10 = (row (take 7 s)*8) + col (drop 7 s)
  | otherwise = -1

col :: String -> Int 
col s = bsp s [0..7]

row :: String -> Int
row s = bsp s [0..127]

bsp :: String -> [Int] -> Int
bsp [] (x:[]) = x
bsp (c:s) x = case c of
                    'F' -> bsp s $ take (length x `div` 2) x
                    'L' -> bsp s $ take (length x `div` 2) x
                    'B' -> bsp s $ drop (length x `div` 2) x
                    'R' -> bsp s $ drop (length x `div` 2) x
