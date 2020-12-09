import Data.List.Split
import Data.List

main = do
  lines <- readFile "input"
  let groups = splitOn "\n\n" lines
  let sgroups = map removeEmptyElems $ map (splitOn "\n") groups
  let commons = map getCommon sgroups
  let ans = sum $ map length commons
  putStrLn $ show ans

removeEmptyElems [] = []
removeEmptyElems ("":s) = removeEmptyElems s
removeEmptyElems (x:s) = x: removeEmptyElems s

getCommon :: [String] -> String
getCommon s = nub [c | x<-s, c<-x, isInAll c s]

isInAll :: Char -> [String] -> Bool
isInAll c s = all (==True) [c `elem` x | x<-s]
