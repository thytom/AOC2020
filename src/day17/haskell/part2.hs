{- Start with the given cubes as a list.

  -}

--          ( x ,  y ,  z ,  w )
type Cube = (Int, Int, Int, Int, Bool)
type Space = [Cube]

-- Boundaries of a dimension
type Bound = (Int, Int)

main = do 
  f <- readFile "input"
  let start = parseState f
  let end = iterate step start !! 6
  putStrLn $ show $ length $ filter (\(_, _, _, _, b)->b) $ end

parseState :: String -> Space
parseState input = concat [[(x, y, 0, 0, state h)| (y, h) <- zip [0..] l] | (x, l)<-zip [0..] $ lines input]
  where 
    state c 
      | c == '#' = True
      | c == '.' = False

step :: Space -> Space
step s = map (\x-> nextState x $ getNeighbors x m) m
  where m = pad s

-- Take a space, add 1 cube width of padding in all dimensions.
pad :: Space -> Space
pad s = s ++ [(x, y, z, w, False) | x<-[x1-1..x2+1], y<-[y1-1..y2+1], z<-[z1-1..z2+1], w<-[w1-1..w2+1], not $ cubeIn (x, y, z, w) s ]
  where
   ((x1, x2), (y1, y2), (z1, z2), (w1, w2)) = getBounds s

cubeIn :: (Int, Int, Int, Int) -> Space -> Bool
cubeIn (x, y, z, w) s = (x, y, z, w, True) `elem` s || (x, y, z, w, False) `elem` s

-- Given a space of points, get the bounds of that space. 
getBounds :: Space -> (Bound, Bound, Bound, Bound)
getBounds space = (bounds x, bounds y, bounds z, bounds w)
  where
    (x, y, z, w) = foldr (\(x, y, z, w, _) (xs, ys, zs, ws) -> (x:xs, y:ys, z:zs, w:ws)) ([], [], [], []) space
    bounds ns = (minimum ns, maximum ns)

nextState :: Cube -> [Cube] -> Cube
nextState (x, y, z, w, False) xs = (x, y, z, w, activeneighbors == 3)
  where
    activeneighbors = length (filter (\(_, _, _, _, x) -> x) xs) 
nextState (x, y, z, w, True) xs 
  | activeneighbors == 2 || activeneighbors == 3 = (x, y, z, w, True)
  | otherwise = (x, y, z, w, False)
  where
    activeneighbors = length (filter (\(_, _, _, _, x) -> x) xs) 

-- Takes a cube and a space, returns its neighbors
getNeighbors :: Cube -> Space -> [Cube]
getNeighbors c s = filter (isNeighbor c) s

isNeighbor :: Cube -> Cube -> Bool
isNeighbor (x1, y1, z1, w1, _) (x2, y2, z2, w2, _) = (==1) $ maximum [diff x1 x2, diff y1 y2, diff z1 z2, diff w1 w2]

diff :: Int -> Int -> Int
diff x y = abs (x - y)
