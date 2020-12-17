import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type Range = (Int, Int)
type Ticket = [Int]
type Rule = (String, [Range])

main = do
  f <- readFile "input"
  let [d_properties, _:d_mine, _:d_others] = map lines $ splitOn "\n\n" f
  let properties = map (\[x, y]->(x, convertToRange y)) $ map (splitOn ": ") d_properties
  let mine = map (read :: String -> Int)$ splitOn "," $ concat d_mine
  let others = map (map (read :: String -> Int) . splitOn ",")$ d_others
  putStrLn $ show $ sum $ map (\(x, b)->x)$ filter (\(_,b)-> b==False)$ map (\x-> (x, chk x properties)) $ concat $ others

checkTicket :: [Rule] -> Ticket -> Bool
checkTicket rules t = any (==True) $ map (check t) rules

check :: Ticket -> Rule -> Bool
check t (r, xs) = any (==True) $ concat $ [[between n x y | (x,y)<-xs] | n <- t]

chk :: Int -> [Rule] -> Bool
chk i rs = any (==True) $ concat $ [[between i x y | (x, y)<-xs] | (_ ,xs)<-rs]

between :: Int -> Int -> Int -> Bool
between n x y = x <= n && n <=y

convertToRange :: String -> [Range]
convertToRange x = map (\[x, y]->(read x :: Int, read y :: Int)) $ map (splitOn "-") $ splitOn " or " x
