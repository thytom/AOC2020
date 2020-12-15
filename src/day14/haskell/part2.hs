import Data.Char
import Text.Regex

-- Mask is stored as a pair of numbers.
-- The first is the number to OR, the
-- second is the number to AND.
type Mask = [[Char]]

-- Address, Value
type MemAddr = (Int, Int)

main = do
  f <- readFile("input")
  let l = lines f
  let result = sum $ map (\(a, b) -> b) $ getUniques [] $ reverse $ concat $ map indiv $ splitSections l
  putStrLn $ show $ result

indiv xs = assigns
  where
    (maskdata:memdata) = xs
    mask = createMask maskdata
    assigns = concat $ map (\x -> applyMask x mask) $ parseAddresses memdata

createMask :: String -> Mask
createMask [] = []
createMask (x:xs) 
  | x == '1' = ['1']:createMask xs
  | x == '0' = [' ']:createMask xs
  | x == 'X' = ['0', '1']:createMask xs
  | otherwise = createMask xs

applyMask :: MemAddr -> Mask -> [MemAddr]
applyMask (a, b) mask = [(a .|. x, b) | x<-getMaskCombinations (mask) [[]]]

(.@.) x mask = zip mask (padTo36 $ toBinary x)

(.|.) :: Int -> String -> Int
(.|.) x mask = convert [orr m b | (m, b)<-zip mask (padTo36 $ toBinary x)]

orr m bit 
  | m == '1' = '1'
  | m == '0' = '0'
  | m == ' ' = bit

toBinary :: Int -> String
toBinary 0 = ['0']
toBinary n = toBinary ( n `quot` 2 ) ++ (show $ n `rem` 2 )

padTo36 :: String -> String
padTo36 s = replicate (36 - length s) '0' ++ s

getMaskCombinations :: Mask -> [[Char]] -> [[Char]]
getMaskCombinations [] xs = map (reverse) xs
getMaskCombinations (m:ms) xs = getMaskCombinations ms [i:x | x<-xs, i<-m]

-- Converts a binary string into a decimal number
convert :: String -> Int
convert = conv . reverse

conv :: String -> Int
conv [] = 0
conv (x:xs) = (digitToInt x) + 2 * conv xs

parseAddresses :: [String] -> [MemAddr]
parseAddresses = map addrFromString

addrRegex = mkRegex "mem\\[|] = "

addrFromString :: String -> MemAddr
addrFromString s = (address, value)
  where
    match = drop 1 $ splitRegex addrRegex s
    address = read $ head match :: Int 
    value   = read $ last match :: Int

getUniques :: [MemAddr] -> [MemAddr] -> [MemAddr]
getUniques ys [] = ys
getUniques ys ((a, b):xs) = if any (==True) [a == x | (x,_)<-ys]
                               then getUniques ys xs
                               else getUniques ((a, b):ys) xs

splitSections :: [String] -> [[String]]
splitSections [] = []
splitSections (x:xs) =  (x:z):splitSections (drop (length z) xs)
  where z = takeWhile (not.isMaskLine) xs

isMaskLine :: [Char] -> Bool
isMaskLine ('m':'a':'s':'k':_) = True
isMaskLine _ = False

