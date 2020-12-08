import Data.List.Split
import Data.Text (isInfixOf, pack)
main = do
  input <- readFile "input"
  let l =  map (map (\x -> if x /= '\n' then x else ' ')) $ splitOn "\n\n" input
  let valids = filter (isValid) l
  putStrLn $ show $ length $ filter (isValid) l

validentries = map pack ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
isValid s = all (==True) [x `isInfixOf` (pack s) | x <- validentries]
