module Day_3 where

level :: String -> String
level pattern = pattern ++ level pattern

toMountain :: [String] -> [String]
toMountain = map level

data Slope = Slope {
    right :: Int,
    down :: Int
}

countTrees :: [String] -> Slope -> Int
countTrees levels slope = countTrees' 0 (down slope) levels slope

countTrees' :: Int -> Int -> [String] -> Slope -> Int
countTrees' _ _ [] _ = 0
countTrees' x 0 levels slope =
    let
        (Slope right down) = slope
        newX = (x + right)
        (level:_) = levels
        (ch:_) = drop (x + right) level
        treeCount = if isTree ch then 1 else 0
    in
        treeCount + countTrees' newX down levels slope
countTrees' x downCount (_:otherLevels) slope = countTrees' x (downCount - 1) otherLevels slope

isTree :: Char -> Bool
isTree '#' = True
isTree _ = False

treeAt :: Int -> String -> Bool
treeAt _ [] = False
treeAt 0 (ch:_) = isTree ch
treeAt index (_:rest) = treeAt (index - 1) rest

main :: IO ()
main = do
    text <- readFile "3.txt"
    let mountain = toMountain (lines text)
    let result = product $ map (countTrees mountain) [Slope 1 1, Slope 3 1, Slope 5 1, Slope 7 1, Slope 1 2]
    print result
