import Text.Regex (splitRegex, mkRegex)

main = do
  input <- readFile "input"
  let parsed = map (splitRegex (mkRegex "-| |: ")) $ lines input
  putStrLn $ show $ length $ filter (validPassword) parsed

validPassword [mn, mx, (c:[]), p] = (c == (p!!a)) /= (c == (p!!b))
  where 
      a = (read mn :: Int) - 1
      b = (read mx :: Int) - 1
