module Day_1_2 where

withIndex :: [Int] -> [(Int, Int)]
withIndex = zip [0..]

expand' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int, Int, Int, Int)]
expand' list1 list2 list3 =
    [ (x_i, y_i, z_i, x_val, y_val, z_val) | (x_i, x_val) <- list1, (y_i, y_val) <- list2, (z_i, z_val) <- list3]

expand :: [Int] -> [(Int, Int, Int, Int, Int, Int)]
expand ints = expand' intsWithIndex intsWithIndex intsWithIndex
    where intsWithIndex = withIndex ints

differentIndices :: (Int, Int, Int, Int, Int, Int) -> Bool
differentIndices (x_i, y_i, z_i, _, _, _) = x_i /= y_i && y_i /= z_i && x_i /= z_i

expectedSum :: Int -> (Int, Int, Int, Int, Int, Int) -> Bool
expectedSum expected (_, _, _, x_val, y_val, z_val) = (x_val + y_val + z_val) == expected

productOfValues :: (Int, Int, Int, Int, Int, Int) -> Int
productOfValues (_, _, _, x_val, y_val, z_val) = x_val * y_val * z_val

main :: IO ()
main = do
    text <- readFile "1.txt"
    let values = map (\token -> read token :: Int) (lines text)
    let result = productOfValues $ head $ filter (expectedSum 2020) $ filter differentIndices $ expand values
    print $ show result
