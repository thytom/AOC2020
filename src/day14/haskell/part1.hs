import Data.Bits
import Data.Char
import Text.Regex

-- Mask is stored as a pair of numbers.
-- The first is the number to OR, the
-- second is the number to AND.
type Mask = (Int, Int)

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
    assigns = map (\(a, b) -> (a,applyMask b mask)) $ parseAddresses memdata

-- createMask :: String -> Mask
createMask s = (ornum, andnum)
  where
    z = subRegex (mkRegex "mask = ") s ""
    ornum  = convert $ map (\x -> if x == '1' then '1' else '0') z
    andnum = convert $ map (\x -> if x == '0' then '0' else '1') z

applyMask :: Int -> Mask -> Int
applyMask n (x,y) = (n .|. x) .&. y

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

