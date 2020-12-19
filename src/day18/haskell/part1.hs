-- Define Types

data Op = Mul | Add
  deriving (Eq, Ord)

instance Show Op where
  show Add = "+"
  show Mul = "*"

data Expr = Term Int | Op Op | Paren Char
  deriving (Eq)

instance Show Expr where
  show (Term x) = show x
  show (Op x)   = show x
  show (Paren x) = show x

-- Evaluation

eval :: Op -> (Int -> Int -> Int)
eval Add = (+)
eval Mul = (*)

rpn :: [Expr] -> Int
rpn x = rpn' x []
  where
    rpn' :: [Expr] -> [Int] -> Int
    rpn' [] stack = head stack
    rpn' (Term x:xs) stack = rpn' xs (x:stack)
    rpn' (Op x:xs)   (i:j:stack) = rpn' xs ((eval x i j):stack)

-- Parsing

parse :: String -> [Expr]
parse = map parseSingle . removeSpaces
  where
    parseSingle x = case x of
                      '+' -> Op Add
                      '*' -> Op Mul 
                      '(' -> Paren x
                      ')' -> Paren x
                      _   -> Term (read [x] :: Int)
    removeSpaces xs = [x | x<-xs, x /= ' ']

-- Converting infix to postfix

postfix :: [Expr] -> [Expr]
postfix = postfix' [] []
  where
    postfix' :: [Expr] -> [Expr] -> [Expr] -> [Expr]
    postfix' out op        (Term x:xs)    = postfix' (out ++ [Term x]) op xs
    postfix' out op        (Paren '(':xs) = postfix' out (Paren '(':op) xs
    postfix' out (Op y:op) (Op x:xs)      = postfix' (out ++ [Op y]) (Op x:op) xs
    postfix' out op        (Op x:xs)      = postfix' out (Op x:op) xs
    postfix' out (Op x:op) (Paren ')':xs) = postfix' (out ++ [Op x]) op (Paren ')':xs)
    postfix' out (Paren '(':op) (Paren ')':xs) = postfix' out op xs
    postfix' out op [] = out ++ op

-- Calculation

calculate :: String -> Int
calculate = rpn . postfix . parse

-- Main

main = do
  f <- readFile "input"
  putStrLn $ (show . sum . map calculate . lines) f
