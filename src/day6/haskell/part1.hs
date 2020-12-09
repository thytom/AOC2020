import Data.List.Split
import Data.List

main = do
  lines <- readFile "input"
  let groups = splitOn "\n\n" lines
  let sgroups = map scrubnewlines groups
  let uniqgroups = map nub sgroups
  let lengths = map length uniqgroups
  putStrLn $ show $ sum lengths 

scrubnewlines [] = []
scrubnewlines (x:s)
  | x == '\n' = scrubnewlines s
  | otherwise = x : scrubnewlines s
