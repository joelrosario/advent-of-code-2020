module Day_4 where

import Data.List.Split

splitPassports :: [String] -> [[String]]
splitPassports = foldl 
                    (\ (passport:rest) str -> case str of
                        ""  -> []:(passport:rest)
                        _   -> (passport ++ [str]):rest) [[]]

-- replace :: String -> Char -> Char -> String
-- replace [] _ _ = []
-- replace (ch:rest) ch1 ch2 = (if ch == ch1 then ch2 else ch):replace rest ch1 ch2

-- splitOn :: String -> Char -> (String, String)
-- splitOn fragment ch = (word1, word2)
--     where [word1, word2] = words (replace fragment ch ' ')

toPair :: String -> (String, String)
toPair fragment = (part1, part2)
    where [part1, part2] = splitOn ":" fragment

toPairs :: [String] -> [(String, String)]
toPairs passport =
    let
        fragments = concatMap words passport
    in map toPair fragments

between :: Int -> Int -> Int -> Bool
between val minVal maxVal = val >= minVal && val <= maxVal

validEcl :: String -> Bool
validEcl ecl = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validHcl :: String -> Bool
validHcl [] = False
validHcl ('#':rest) = length (filter (`elem` "0123456789abcdef") rest) == length rest
validHcl _ = False

isNumber :: String -> Bool
isNumber value = length (filter (`elem` "1234567890") value) == length value

hgt :: String -> Bool
hgt value =
    let reversedValue = reverse value
    in case reversedValue of
        ('m':'c':rest) -> isNumber rest && between (read (reverse rest) :: Int) 150 193
        ('n':'i':rest) -> isNumber rest && between (read (reverse rest) :: Int) 59 76
        _ -> False

validToken :: (String, String) -> Bool
validToken (key, value) = case key of
    "byr" -> length value == 4 && range 1920 2002
    "iyr" -> length value == 4 && range 2010 2020
    "eyr" -> length value == 4 && range 2020 2030
    "hgt" -> hgt value
    "hcl" -> validHcl value
    "ecl" -> validEcl value
    "pid" -> length value == 9 && isNumber value
    _ -> False
    where
        intValue = read value :: Int
        range = between intValue 

tokenValue :: (String, String) -> Int
tokenValue token = if validToken token then 1 else 0

validPassport :: [(String, String)] -> Bool
validPassport passport = total == 7
    where
        total = sum $ map tokenValue passport

parsePassports :: String -> [[(String, String)]]
parsePassports text =
    let
        passportLines = lines text
        rawPassports = splitPassports passportLines
    in map toPairs rawPassports

main :: IO ()
main = do
    text <- readFile "4.txt"
    let passports = parsePassports text
    let validPassportCount = length $ filter validPassport passports
    print validPassportCount
