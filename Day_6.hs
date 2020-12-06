{-# OPTIONS_GHC -Wall #-}

module Day_6 where

import Data.List.Split ( splitOn )
import Data.Set (fromList, toList, intersection, size)

main :: IO ()
main = do
    text <- readFile "6.txt"
    let answerGroups = parseAnswersGroups text
    putStrLn $ "Anyone answered yes: " ++ show (sum $ anyoneAnsweredYes answerGroups)
    putStrLn $ "Everyone answere yes: " ++ show (sum $ everyoneAnsweredYes answerGroups)

everyoneAnsweredYes' :: [String] -> Int
everyoneAnsweredYes' [] = 0
everyoneAnsweredYes' answerGroup = size $ foldl1 intersection $ map fromList answerGroup

everyoneAnsweredYes :: [[String]] -> [Int]
everyoneAnsweredYes = map everyoneAnsweredYes'

anyoneAnsweredYes :: [[String]] -> [Int]
anyoneAnsweredYes = map (length . unique . concat)

unique :: String -> String
unique text = toList $ fromList text

parseAnswersGroups :: String -> [[String]]
parseAnswersGroups text = map (splitOn "\n") $ splitOn "\n\n" text
