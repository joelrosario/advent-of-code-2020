{-# OPTIONS_GHC -Wall #-}

module Day_9 where

import Data.Sort

combinationsOf :: Eq a => [a] -> [(a, a)]
combinationsOf [] = []
combinationsOf (value:rest) =
    filter (\(x, y) -> x /= y) (map (\n -> (value, n)) rest) ++ combinationsOf rest

invalid :: Int -> [Int] -> Bool
invalid value preamble =
    let results = filter (\(x, y) -> value == x + y) (combinationsOf preamble)
    in case results of
        [] -> True
        _  -> False

data Result = AllValid | Invalid Int Int
    deriving (Show)

successiveSegments :: [a] -> Int -> [[a]]
successiveSegments [] _ = []
successiveSegments values size | size <= length values = (take size values):(successiveSegments (drop 1 values) size)
                               | otherwise = []

contiguousSegments :: [a] -> Int -> Int -> [[a]]
contiguousSegments [] _ _ = []
contiguousSegments values size maxSize
    | size > maxSize = []
    | otherwise = successiveSegments values size ++ contiguousSegments values (size + 1) maxSize

contiguousSegment :: Int -> [Int] -> [Int]
contiguousSegment value values =
    let segments = contiguousSegments values 2 (length values)
    in head $ filter (\segment -> value == sum segment) segments

encryptionWeakness :: Int -> [Int] -> Int
encryptionWeakness value preamble = 
    let segment = sort $ contiguousSegment value preamble
    in if length segment >= 2 then head segment + last segment else -1

findInvalidNumber :: [Int] -> [Int] -> Int -> Result
findInvalidNumber originalValues values preambleSize
    | length values <= preambleSize = AllValid
    | invalid value preamble        = Invalid value (encryptionWeakness value originalValues)
    | otherwise                     = findInvalidNumber originalValues (drop 1 values) preambleSize
        where
            preamble = take preambleSize values
            (value:_) = drop preambleSize values

main :: IO ()
main = do
    text <- readFile "9.txt"
    let values = map (\n -> read n :: Int) (lines text)
    let invalidNumber = findInvalidNumber values values 25
    print invalidNumber
