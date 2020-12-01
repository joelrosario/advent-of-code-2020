module Day_1 where

withIndex :: [Int] -> [(Int, Int)]
withIndex = zip [0..]

expand' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int, Int)]
expand' list1 list2 =
    [ (x_i, y_i, x_val, y_val) | (x_i, x_val) <- list1, (y_i, y_val) <- list2]

expand :: [Int] -> [(Int, Int, Int, Int)]
expand ints = expand' intsWithIndex intsWithIndex
    where intsWithIndex = withIndex ints

differentIndices :: (Int, Int, Int, Int) -> Bool
differentIndices (x_i, y_i, _, _) = x_i /= y_i

expectedSum :: Int -> (Int, Int, Int, Int) -> Bool
expectedSum expected (_, _, x_val, y_val) = (x_val + y_val) == expected

main :: IO ()
main = do
    text <- readFile "1.txt"
    let values = map (\token -> read token :: Int) (lines text)
    let (_, _, x, y) = head $ filter (expectedSum 2020) $ filter differentIndices $ expand values
    print $ show (x * y)
