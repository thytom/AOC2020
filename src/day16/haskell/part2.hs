import Data.List.Split (splitOn)
import Data.Function (on)
import Data.List (nub, sortBy, isInfixOf)
import qualified Data.Map.Strict as M

type Range = (Int, Int)
type Ticket = [Int]
type Rule = (String, [Range])

main = do
  f <- readFile "input"
  let [d_properties, _:d_mine, _:d_others] = map lines $ splitOn "\n\n" f
  let properties = map (\[x, y]->(x, convertToRange y)) $ map (splitOn ": ") d_properties
  let mine       = map (read :: String -> Int)$ splitOn "," $ concat d_mine
  let others     = map (map (read :: String -> Int) . splitOn ",")$ d_others
  let noinvalid  = filter (\x -> all (==True) $ map (valid properties) x) others
  let propcheck  = ticketsToProperties noinvalid
  let propnames  = map (\x -> ([n | r@(n,_)<-properties, checkAll x r])) propcheck
  let propreduces = map (\(x, [y]) -> (x, y))$ sortBy (compare `on` fst) $ reduce [] $ zip [1..] propnames 
  let depints = map (\(x,_) -> x) $ filter (\(x, y) -> "departure" `isInfixOf` y) $ propreduces
  putStrLn $ show $ product $ getVals mine depints

-- Takes list of ints and list of positions.
getVals :: Ticket -> [Int] -> [Int]
getVals ticket xs = [x | (x, n)<-zip ticket [1..], n `elem` xs]

reduce :: [String] -> [(Int, [String])] -> [(Int, [String])]
reduce u xs 
  | length uniques == 20 = xs
  | otherwise = reduce (uniques) (elimAll uniques xs)
  where
    uniques = nub $ u ++ [head x| (n, x)<-xs, length x == 1]

elimAll :: [String] -> [(Int, [String])] -> [(Int, [String])]
elimAll [] xs      = xs
elimAll (s:ss) xs  = elimAll ss (elim s xs)

elim :: String -> [(Int, [String])] -> [(Int, [String])]
elim s xs = [(n, filter (/=s) x) | (n, x) <- xs, length x > 1] ++ [(n, x) | (n, x)<-xs, length x == 1]

-- Convert a list of tickets into a list of their properties
ticketsToProperties :: [Ticket] -> [[Int]]
ticketsToProperties ([]:_) = []
ticketsToProperties ts = [head t | t<-ts]:(ticketsToProperties [tail t | t<-ts])

checkAll :: [Int] -> Rule -> Bool
checkAll ns r = all (==True) [chk n r | n<-ns]

-- Is this number valid in any rule
valid :: [Rule] -> Int -> Bool
valid rs i = any (==True) [chk i r | r <- rs]

chk :: Int -> Rule -> Bool
chk i (_, xs) = any (==True) [between i x y | (x, y)<-xs]

between :: Int -> Int -> Int -> Bool
between n x y = x <= n && n <=y

convertToRange :: String -> [Range]
convertToRange x = map (\[x, y]->(read x :: Int, read y :: Int)) $ map (splitOn "-") $ splitOn " or " x
