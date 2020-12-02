module Day_2 where

data PasswordPolicy = PasswordPolicy {
    val1 :: Int,
    val2 :: Int,
    letter :: Char
}
    deriving (Show)

data PasswordRecord = PasswordRecord {
    policy :: PasswordPolicy,
    password :: String
}
    deriving (Show)

splitOn :: Char -> String -> (String, String)
splitOn splitter (ch:rest)
    | splitter == ch = ("", rest)
    | otherwise =
        let
            (first, remainder) = splitOn splitter rest
        in
            (ch:first, remainder)

getMin :: String -> (Int, String)
getMin text =
    let (first, remainder) = splitOn '-' text
    in (read first :: Int, remainder)

getMax :: String -> (Int, String)
getMax text =
    let (first, remainder) = splitOn ' ' text
    in (read first :: Int, remainder)

getLetter :: String -> (Char, String)
getLetter text =
    let (first, remainder) = splitOn ':' text
    in (head first, remainder)

getPassword :: String -> String
getPassword = drop 1

parsePasswordRecord :: String -> PasswordRecord
parsePasswordRecord recordStr =
    let
        (minCount, rest1) = getMin recordStr
        (maxCount, rest2) = getMax rest1
        (letter, rest3) = getLetter rest2
        password = getPassword rest3
    in PasswordRecord (PasswordPolicy minCount maxCount letter) password

countLetter :: Char -> String -> Int
countLetter _ [] = 0
countLetter letter (ch:rest) =
    let count = if letter == ch then 1 else 0
    in count + countLetter letter rest

matchesPolicy1 :: PasswordRecord -> Bool
matchesPolicy1 (PasswordRecord (PasswordPolicy minCount maxCount letter) password) =
    let count = countLetter letter password
    in count >= minCount && count <= maxCount

matchesPolicy2 :: PasswordRecord -> Bool
matchesPolicy2 (PasswordRecord (PasswordPolicy pos1 pos2 letter) password) =
    let
        letter1 = letterAt pos1 password
        letter2 = letterAt pos2 password
        leftMatch = letter1 == letter && letter2 /= letter
        rightMatch = letter1 /= letter && letter2 == letter
    in leftMatch || rightMatch

letterAt :: Int -> String -> Char
letterAt index = letterAt' index 1

letterAt' :: Int -> Int -> String -> Char
letterAt' index current (ch:rest)
    | index == current = ch
    | otherwise = letterAt' index (current + 1) rest

main1 :: IO ()
main1 = do
    text <- readFile "2.txt"
    let passwordLines = lines text
    let len = length $ filter matchesPolicy1 $ map parsePasswordRecord passwordLines
    print len

main2 :: IO ()
main2 = do
    text <- readFile "2.txt"
    let passwordLines = lines text
    let len = length $ filter matchesPolicy2 $ map parsePasswordRecord passwordLines
    print len
