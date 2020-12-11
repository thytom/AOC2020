import Data.List (intercalate)

data Object = Free | Occupied | Floor
  deriving (Eq, Show)

type Map = [[Object]]

main = do
  f <- readFile "input"
  let l = lines f
  let initialMap = map parseLine $ lines f
  putStrLn $ show $ length $ filter (=='#') $ stringMap $ equilibrium [] $ initialMap

parseLine :: String -> [Object]
parseLine = map charToObject 

charToObject :: Char -> Object
charToObject 'L' = Free
charToObject '#' = Occupied
charToObject '.' = Floor

getAdjacents :: (Int, Int) -> Map -> [Object]
getAdjacents (row, col) map = [map!!r!!c | (r,c)<-validCoords]
  where validCoords = [(r, c) | r<-[row-1..row+1], c<-[col-1..col+1]
                      , r >= 0, r < length map, c >= 0, c < (length (map!!0))
                      , (r, c) /= (row, col)]

step :: Map -> Map
step m = [[nextObject (m!!r!!c) (getAdjacents (r,c) m) | c<-[0..(length $ m!!0) -1] ] | r<-[0..(length m)-1]]

getMaxRowCol :: Map -> (Int, Int)
getMaxRowCol map = ((length map) -1, (length $ map!!0) -1 )

equilibrium :: Map -> Map -> Map
equilibrium x y 
  | x == y = y
  | otherwise = equilibrium y $ step y

-- Get next object from its current state and adjacents
nextObject :: Object -> [Object] -> Object
nextObject Free adj
  | length (filter (==Occupied) adj) <= 0 = Occupied
  | otherwise = Free
nextObject Occupied adj
  | length (filter (==Occupied) adj) >= 4 = Free
  | otherwise = Occupied
nextObject o _ = o

stringMap :: Map -> String
stringMap = intercalate "\n" . map (map objectToChar)

objectToChar :: Object -> Char
objectToChar Free     = 'L'
objectToChar Occupied = '#'
objectToChar Floor    = '.'
