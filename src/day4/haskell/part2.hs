import Data.List.Split
import Text.Regex
import Data.Text (isInfixOf, pack)
main = do
  input <- readFile "input"
  let l =  map (map (\x -> if x /= '\n' then x else ' ')) $ splitOn "\n\n" input
  let valids = filter (isValid) l
  let parsed = map (map (splitOn ":") ) $ map (splitOn " ") valids
  putStrLn $ show $ length $ filter (deepValid) parsed

isValid s = all (==True) [x `isInfixOf` (pack s) | x <- validentries]
  where validentries = map pack ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

deepValid :: [[String]] -> Bool
deepValid s = all (==True) $ map check s
  where
    check :: [String] -> Bool
    check [x, v] = case x of
                "byr" -> between 1920 2002 $ toInt v
                "iyr" -> between 2010 2020 $ toInt v
                "eyr" -> between 2020 2030 $ toInt v
                "hgt" -> checkHeight v
                "hcl" -> matchRegex (mkRegex "^#[0-9,a-z]{6}$") v /= Nothing
                "ecl" -> v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
                "pid" -> matchRegex (mkRegex "^[0-9]{9}$") v /= Nothing
                _ -> True
    toInt x = read x :: Int
    between x y v = v <= y && v >= x
    checkHeight v = if ((pack "in")`isInfixOf`(pack v)) 
                       then between 59 76 $ getHeight v
                    else if ((pack "cm") `isInfixOf` (pack v))
                       then between 150 193 $ getHeight v
                    else False

    getHeight x = read (subRegex (mkRegex "in|cm") x "") :: Int

