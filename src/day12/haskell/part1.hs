type Position = (Int, Int, Int)
type Command = (Char, Int)

main = do
  f <- readFile "input"
  let l = map parseCommand $ lines f
  putStrLn $ show $ manhattan $ applyMoves l (0, 0, 90)

manhattan :: Position -> Int
manhattan (x, y, _) = abs x + abs y

applyMoves :: [Command] -> Position -> Position
applyMoves [] pos = pos
applyMoves (x:xs) pos = applyMoves xs (move x pos)

move :: Command -> Position -> Position
move (c, amt) (x, y, r) 
  | c == 'N' = (x, y+amt, r)
  | c == 'S' = (x, y-amt, r)
  | c == 'E' = (x+amt, y, r)
  | c == 'W' = (x-amt, y, r)
  | c == 'F' = move (getdir r, amt) (x, y, r)
  | c == 'L' = (x, y, (r-amt) `mod` 360)
  | c == 'R' = (x, y, (r+amt) `mod` 360)
  | otherwise = (x, y, r)
    where
      getdir r
        | r == 0 = 'N'
        | r == 90 = 'E'
        | r == 180 = 'S'
        | r == 270 = 'W'

parseCommand :: String -> Command
parseCommand (x:xs) = (x, read xs :: Int)
