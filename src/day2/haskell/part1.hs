import Text.Regex (splitRegex, mkRegex)

main = do
  input <- readFile "input"
  let parsed = map (splitRegex (mkRegex "-| |: ")) $ lines input
  putStrLn $ show $ length $ filter (validPassword) parsed

validPassword [mn, mx, (c:[]), p] = cnt >= min && cnt <= max
  where cnt = length $ filter (==c) p
        min = read mn :: Int
        max = read mx :: Int
