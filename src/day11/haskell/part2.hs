import Data.List (intercalate)

data Object = Free | Occupied | Floor
  deriving (Eq, Show)

type Map = [[Object]]

main = do
  f <- readFile "input"
  let l = lines f
  let initialMap = map parseLine $ lines f
  -- putStrLn $ show $ getAdjacents (0, 0) initialMap
  -- putStrLn $ stringMap $ step.step $ initialMap
  putStrLn $ show $ length $ filter (=='#') $ stringMap $ equilibrium [] $ initialMap

parseLine :: String -> [Object]
parseLine = map charToObject 

charToObject :: Char -> Object
charToObject 'L' = Free
charToObject '#' = Occupied
charToObject '.' = Floor

safehead [] = Floor
safehead (x:_) = x

getAdjacents :: (Int, Int) -> Map -> [Object]
getAdjacents (row, col) map = [safehead $ dropWhile (==Floor) $ getObjectsInDirection d (row,col) map | d<-directions]
  where
    directions = [(x, y) | x<-[-1..1], y<-[-1..1], x/=0 || y/=0]

getObjectsInDirection :: (Int, Int) -> (Int, Int) -> Map -> [Object]
getObjectsInDirection (dx, dy) (x, y) map = checkDirection (x+dx, y+dy)
  where 
    checkDirection :: (Int, Int) -> [Object]
    checkDirection (x, y)
          | isPointOutOfBounds (x, y) map = []
          | otherwise = map!!x!!y : checkDirection (x+dx, y+dy)

isPointOutOfBounds (x, y) map = x < 0 || x > mx || y < 0 || y > my
  where (mx, my) = getMaxRowCol map

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
  | length (filter (==Occupied) adj) >= 5 = Free
  | otherwise = Occupied
nextObject o _ = o

stringMap :: Map -> String
stringMap = intercalate "\n" . map (map objectToChar)

objectToChar :: Object -> Char
objectToChar Free     = 'L'
objectToChar Occupied = '#'
objectToChar Floor    = '.'
