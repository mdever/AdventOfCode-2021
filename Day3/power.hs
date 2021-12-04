{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.Text (isPrefixOf, pack)
import Data.Bits

toInt :: String -> Int
toInt = read

data BitCounter = BitCounter { bit11 :: Int, bit10 :: Int, bit9 :: Int, bit8 :: Int,
                               bit7 :: Int, bit6 :: Int, bit5 :: Int, bit4 :: Int,
                               bit3 :: Int, bit2 :: Int, bit1 :: Int, bit0 :: Int } deriving (Show)

mapCounter :: (Int -> a) -> BitCounter -> [a]
mapCounter f counter = (f (bit11 counter)) : (f (bit10 counter)) : (f (bit9 counter)) : (f (bit8 counter)) : (f (bit7 counter)) :
                       (f (bit6 counter))  : (f (bit5  counter)) : (f (bit4 counter)) : (f (bit3 counter)) : (f (bit2 counter)) :
                       (f (bit1 counter))  : (f (bit0 counter))  : []

initialBitCounter = BitCounter 0 0 0 0 0 0 0 0 0 0 0 0

charMap :: (Char -> a) -> String -> [a]
charMap f ""  = []
charMap f str = let c = (str !! 0) in (f c) : charMap f (tail str)


bitStringToInt :: String -> Int
bitStringToInt bitStr = stringToBits' (charMap digitToInt (reverse bitStr)) (0 :: Int) 0
  where stringToBits' (b:bs) prev idx = if b > 0
                                        then stringToBits' bs (setBit prev idx) (idx+1)
                                        else stringToBits' bs prev (idx+1)
        stringToBits' [] prev _ = prev

getReadings :: IO [Int]
getReadings = do
    input <- getContents
    let ls = map bitStringToInt $ lines input
    return ls

countBits :: [Int] -> BitCounter
countBits numbers = foldl (\counter nextNum -> let b0  = if testBit nextNum 0  then (bit0  counter) + 1 else (bit0  counter - 1)
                                                   b1  = if testBit nextNum 1  then (bit1  counter) + 1 else (bit1  counter - 1)
                                                   b2  = if testBit nextNum 2  then (bit2  counter) + 1 else (bit2  counter - 1)
                                                   b3  = if testBit nextNum 3  then (bit3  counter) + 1 else (bit3  counter - 1)
                                                   b4  = if testBit nextNum 4  then (bit4  counter) + 1 else (bit4  counter - 1)
                                                   b5  = if testBit nextNum 5  then (bit5  counter) + 1 else (bit5  counter - 1)
                                                   b6  = if testBit nextNum 6  then (bit6  counter) + 1 else (bit6  counter - 1)
                                                   b7  = if testBit nextNum 7  then (bit7  counter) + 1 else (bit7  counter - 1)
                                                   b8  = if testBit nextNum 8  then (bit8  counter) + 1 else (bit8  counter - 1)
                                                   b9  = if testBit nextNum 9  then (bit9  counter) + 1 else (bit9  counter - 1)
                                                   b10 = if testBit nextNum 10 then (bit10 counter) + 1 else (bit10 counter - 1)
                                                   b11 = if testBit nextNum 12 then (bit11 counter) + 1 else (bit11 counter - 1)
                                                in BitCounter b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0)
                          initialBitCounter numbers

intListToInt :: [Int] -> Int
intListToInt lst = intListToInt' (reverse lst) (0 :: Int) 0
    where
        intListToInt' (i:is) acc idx = if i > 0 then intListToInt' is (setBit acc idx) (idx+1) else intListToInt' is acc (idx+1)
        intListToInt' [] acc _ = acc

getGamma :: BitCounter -> Int
getGamma counter = intListToInt $ mapCounter (\num -> if num > 0 then 1 else 0) counter

getEpsilon :: BitCounter -> Int
getEpsilon counter = intListToInt $ mapCounter (\num -> if num > 0 then 0 else 1) counter

main = do
    readings <- getReadings
    let bitCounts = countBits readings
    putStrLn $ show bitCounts
    let gamma = getGamma bitCounts
    putStrLn $ "Gamma: " ++ (show gamma)
    let epsilon = getEpsilon bitCounts
    putStrLn $ "Epsilon: " ++ (show epsilon)
    let product = gamma * epsilon
    putStrLn $ "Total power: " ++ (show product)
    