{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.Text (isPrefixOf, pack)

toInt :: String -> Int
toInt = read

data Instruction = Up Int | Down Int | Left Int | Right Int | Forward Int | Back Int | Unknown deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction line = case words line of
                          [direction, distance] -> if isPrefixOf "forward" (pack direction)
                                                   then Forward (toInt distance)
                                                   else if isPrefixOf "up" (pack direction)
                                                   then Up (toInt distance)
                                                   else if isPrefixOf "down" (pack direction)
                                                   then Down (toInt distance)
                                                   else Unknown
                          otherwise -> Unknown

getInstructions :: IO [Instruction]
getInstructions = do
    input <- getContents
    let ls = map parseInstruction $ lines input
    return ls

data Position = Position { depth :: Int, forward :: Int, verticalHeading :: Int } deriving (Show)
startingPosition = Position 0 0 0

processInstructions :: [Instruction] -> Position
processInstructions = foldl (\p i -> case i of
                               (Up amount)      -> Position (depth p) (forward p) ((verticalHeading p) - amount)
                               (Down amount)    -> Position (depth p) (forward p) ((verticalHeading p) + amount)
                               (Forward amount) -> Position ((depth p) + (amount * (verticalHeading p))) ((forward p) + amount) (verticalHeading p)
                               otherwise        -> p
                               ) startingPosition

main = do
    instructions <- getInstructions
    let result = processInstructions instructions
    let product = (forward result) * (depth result)
    putStrLn $ "Final product is: " ++ (show product)
