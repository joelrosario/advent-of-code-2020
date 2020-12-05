{-# OPTIONS_GHC -Wall #-}

module Day_5 where

import Data.Sort ( sort )

type BoardingPass = String
type Row = Int
type Col = Int
type Seat = (Row, Col)

calculateRange :: Int -> Int -> Char -> (Int, Int)
calculateRange start end side =
    let diff = end - start + 1
        half = div diff 2
        lower = (start, end - half)
        upper = (start + half, end)
        deadEnd = (-1, 0)
    in case side of
        'L' -> lower
        'F' -> lower
        'R' -> upper
        'B' -> upper
        _   -> deadEnd

findSeat :: Int -> Int -> BoardingPass -> Int
findSeat start _ [] = start
findSeat start end (side:rest) = 
    let (newStart, newEnd) = calculateRange start end side
    in findSeat newStart newEnd rest

seat :: BoardingPass -> Seat
seat boardingPass = (row, col)
    where
        row = findSeat 0 127 (take 7 boardingPass)
        col = findSeat 0 7 (drop 7 boardingPass)

seatId :: Seat -> Int
seatId (row, col) = row * 8 + col

findMySeatId :: [Int] -> Int
findMySeatId [] = -1
findMySeatId [_] = -1
findMySeatId (a:b:rest) | b - a == 2 = a + 1
                        | otherwise  = findMySeatId (b:rest)

main :: IO ()
main = do
    rawData <- readFile "5.txt"
    let boardingPasses = lines rawData
    let seats = map seat boardingPasses
    let seatIds = map seatId seats
    let highestSeatId = maximum seatIds
    print highestSeatId

    let sortedIds = sort seatIds
    let mySeatId = findMySeatId sortedIds
    print mySeatId
