type Position = ((Int, Int), (Int, Int))
type Command = (Char, Int)
type Point = (Int, Int)

main = do
  f <- readFile "input"
  let l = map parseCommand $ lines f
  putStrLn $ show $ manhattan $ applyMoves l ((0, 0), (10, 1))

start :: Position
start = ((0, 0), (10, 1))

manhattan :: Position -> Int
manhattan ((x, y), _) = abs x + abs y

applyMoves :: [Command] -> Position -> Position
applyMoves [] pos = pos
applyMoves (x:xs) pos = applyMoves xs (move x pos)

move :: Command -> Position -> Position
move (c, amt) s@((x, y), (x2, y2))
  | c == 'N' = translate (0,  amt) s
  | c == 'S' = translate (0, -amt) s
  | c == 'E' = translate ( amt, 0) s
  | c == 'W' = translate (-amt, 0) s
  | c == 'F' = translateShip (x2*amt, y2*amt) s
  | c == 'L' = rotate (-amt) s
  | c == 'R' = rotate ( amt) s
  | otherwise = s

translate :: Point -> Position -> Position
translate (dx, dy) (ship, (wx, wy)) = (ship, (wx + dx, wy + dy))

translateShip :: Point -> Position -> Position
translateShip (dx, dy) ((x, y), wp) = ((x+dx, y+dy), wp)

rotate :: Int -> Position -> Position
rotate a (s, (ox, oy)) 
  | a ==   90 || a == -270 = (s, ( oy, -ox))
  | a == - 90 || a ==  270 = (s, (-oy,  ox))
  | a == -180 = (s, (-ox, -oy))
  | a ==  180 = (s, (-ox, -oy))

parseCommand :: String -> Command
parseCommand (x:xs) = (x, read xs :: Int)
