{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day_8 where

import qualified Data.Array as Array
import qualified Data.Set as Set

import Debug.Trace

data Instruction = Nop Int | Acc Int | Jmp Int | Unexpected
    deriving (Show)

data Machine = Machine {
    instructions :: Array.Array Int Instruction,
    executionPointer :: Int,
    accumulator :: Int,
    executionLog :: Set.Set Int,
    instructionLength :: Int
}
    deriving (Show)

initializeMachine :: Array.Array Int Instruction -> Machine
initializeMachine instructionList = Machine {
        instructions = instructionList,
        executionPointer = 0,
        accumulator = 0,
        executionLog = Set.empty,
        instructionLength = len
    }
    where 
        bounds = Array.bounds instructionList
        len = snd bounds - fst bounds + 1

executeInstruction :: Machine -> Machine
executeInstruction
    Machine {instructions, executionPointer, accumulator, executionLog, instructionLength} =
        let
            instruction = (Array.!) instructions executionPointer
            (new_accumulator, new_pointer) = case instruction of
                Nop _      -> (accumulator, executionPointer + 1)
                Acc value  -> (accumulator + value, executionPointer + 1)
                Jmp step   -> (accumulator, executionPointer + step)
                Unexpected -> (accumulator, executionPointer)
        in Machine instructions new_pointer new_accumulator (Set.insert executionPointer executionLog) instructionLength

data ComputationResult = Terminated | InfiniteLoop
    deriving (Eq, Show)

executeProgram :: Machine -> (ComputationResult, Int)
executeProgram machine | Set.member executionPointer executionLog = (InfiniteLoop, accumulator)
                       | executionPointer >= instructionLength    = (Terminated, accumulator)
                       | otherwise                                = executeProgram $ executeInstruction machine
                    where
                        Machine {executionPointer, accumulator, executionLog, instructionLength} = machine

swapInstruction :: Machine -> Int -> Machine
swapInstruction machine index =
    let 
        Machine { instructions = oldInstructions } = machine
        instruction = (Array.!) oldInstructions index
    in case instruction of
        (Nop value) -> machine {instructions = (Array.//) oldInstructions [(index, Jmp value)]}
        (Jmp value) -> machine {instructions = (Array.//) oldInstructions [(index, Nop value)]}
        _           -> machine

programsWithSwappedInstructions :: Machine -> [Machine]
programsWithSwappedInstructions machine = 
    let Machine {instructionLength} = machine
    in map (swapInstruction machine) [1..(instructionLength - 1)]

fixAndRunProgram :: Machine -> Int
fixAndRunProgram machine = 
    snd $ head $ filter (\(result, _) -> result == Terminated) $ map executeProgram (programsWithSwappedInstructions machine)

parseInt :: String -> Int
parseInt ('+':rest) = read rest :: Int
parseInt ('-':rest) = -1 * read rest :: Int
parseInt value = read value :: Int

parseInstruction :: String -> Instruction
parseInstruction line =
    let tokens = words line
        [operation, valueStr] = tokens
        value = parseInt valueStr
    in case operation of
        "nop" -> Nop value
        "acc" -> Acc value
        "jmp" -> Jmp value
        _     -> Unexpected

parseInstructions :: [String] -> Array.Array Int Instruction
parseInstructions textLines =
    let instructions = map parseInstruction textLines
    in Array.listArray (0, length instructions - 1) instructions

main :: IO ()
main = do
    text <- readFile "8.txt"
    let textLines = lines text
    let instructions = parseInstructions textLines
    let machine = initializeMachine instructions
    print (executeProgram machine)
    print (fixAndRunProgram machine)
