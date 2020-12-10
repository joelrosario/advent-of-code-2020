{-# OPTIONS_GHC -Wall #-}

module Day_7 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe ( fromMaybe ) 

type Color = String
type ContainingBagRules = Map.Map Color [Color]

parseContainingBags :: [String] -> [(String, Int)]
parseContainingBags (countStr:color1:color2:_:rest) = (color, count):parseContainingBags rest
    where count = read countStr :: Int
          color = color1 ++ " " ++ color2
parseContainingBags _ = []

parseContainingRule :: String -> ContainingBagRules
parseContainingRule ruleString =
    let tokens = words ruleString
        (color1:color2:_:_:rest) = tokens
        bagColor = color1 ++ " " ++ color2
        containedBags = map fst (parseContainingBags rest)
        entries = [ (key, [bagColor] ) | key <- containedBags ]
    in Map.fromList entries

storeContainingRule :: ContainingBagRules -> ContainingBagRules -> ContainingBagRules
storeContainingRule = Map.unionWith (++)

parseContainingRules :: [String] -> ContainingBagRules
parseContainingRules = foldl (\ruleMap rule -> storeContainingRule (parseContainingRule rule) ruleMap) Map.empty

findHavingNoContainers :: [(Color, Maybe [Color])] -> [Color]
findHavingNoContainers [] = []
findHavingNoContainers ((color, Nothing):rest) = color:findHavingNoContainers rest
findHavingNoContainers (_:rest) = findHavingNoContainers rest

findContainers :: [(Color, Maybe [Color])] -> [Color]
findContainers [] = []
findContainers ((_, Just colors):rest) = colors ++ findContainers rest
findContainers ((_,_):rest) = findContainers rest

findBagsContaining :: [Color] -> ContainingBagRules -> [Color]
findBagsContaining [] _ = []
findBagsContaining colors rules =
    let results = map (\color -> (color, Map.lookup color rules)) colors
        containers = findContainers results
    in
        containers ++ findBagsContaining containers rules

type ContainedBagRules = Map.Map Color [(Color, Int)]

parseContainedBags :: [String] -> [(Color, Int)]
parseContainedBags (countStr:color1:color2:_:rest) =
    let
        color = color1 ++ " " ++ color2
        count = read countStr :: Int
    in (color, count):parseContainedBags rest
parseContainedBags _ = []

parseContainedRule :: String -> ContainedBagRules
parseContainedRule ruleString =
    let
        tokens = words ruleString
        (color1:color2:_:_:rest) = tokens
        bagColor = color1 ++ " " ++ color2
        contained = parseContainedBags rest
    in
        Map.fromList [(bagColor, contained)]

storeContainedRule :: ContainedBagRules -> ContainedBagRules -> ContainedBagRules
storeContainedRule = Map.union

parseContainedRules :: [String] -> ContainedBagRules
parseContainedRules [] = Map.empty
parseContainedRules rules =
    foldl (\acc rule -> storeContainedRule (parseContainedRule rule) acc) Map.empty rules

findContainedBagCount :: Color -> ContainedBagRules -> Int
findContainedBagCount color rules =
    let contained = fromMaybe [] (Map.lookup color rules)
    in case contained of
        [] -> 1
        _  -> 1 + (sum $ map (\(containedColor, count) -> count * findContainedBagCount containedColor rules) contained)

uniqueList :: Ord a => [a] -> [a]
uniqueList = Set.toList . Set.fromList

main :: IO ()
main = do
    text <- readFile "7.txt"
    let textLines = lines text
    let containingRules = parseContainingRules textLines
    let containingBags = uniqueList $ findBagsContaining ["shiny gold"] containingRules
    putStrLn ("Container bags: " ++ show (length containingBags))

    let containedRules = parseContainedRules textLines
    let containedBagCount = (findContainedBagCount "shiny gold" containedRules) - 1
    putStrLn ("Contained bags: " ++ show containedBagCount)
