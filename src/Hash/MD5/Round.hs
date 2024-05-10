module Hash.MD5.Round where

import Prelude hiding (round)
import Matrix
import Hash.MD5.Padding
import Hash.MD5.InitValues
import Data.Char (ord)
import Data.Bits (Bits (testBit))
import Data.Bool (bool)

round :: ([Bool] -> [Bool] -> [Bool] -> [Bool]) -> [[Bool]] -> [[Bool]] -> [[Bool]] -> [Int] -> Int -> [[Bool]]
round f buffers m k shifts i 
    | i == 16 = buffers
    | otherwise = round f opResult m k shifts (i+1)
        where opResult = operation f buffers (m!!i) (k!!i) (shifts!!i)

----- A single operation, part of the 64 operations. 
operation :: ([Bool] -> [Bool] -> [Bool] -> [Bool]) -> [[Bool]] -> [Bool] -> [Bool] -> Int -> [[Bool]]
operation f buffers word k shiftNumber = result
    where 
        a = buffers !! 0
        b = buffers !! 1
        c = buffers !! 2
        d = buffers !! 3
        functionResult = f b c d
        sumA = sumMod32 a functionResult
        sumM = sumMod32 sumA word
        sumK = sumMod32 sumM k
        shiftLeft = rotateLeft sumK shiftNumber
        a' = sumMod32 shiftLeft b 
        result = [d,a',b,c]

--Helper functions
sumMod32 :: [Bool] -> [Bool] -> [Bool]
sumMod32 vec1 vec2 = take 32 $ addWithCarry vec1 vec2 False

addWithCarry :: [Bool] -> [Bool] -> Bool -> [Bool]
addWithCarry [] [] carry = [carry]
addWithCarry [] ys carry = addWithCarry [False] ys carry
addWithCarry xs [] carry = addWithCarry xs [False] carry
addWithCarry (x:xs) (y:ys) carry =
    let sumBit = xor3 x y carry
        newCarry = carryOut x y carry
    in sumBit : addWithCarry xs ys newCarry
  where
    xor3 a b c = (a /= b) /= c
    carryOut a b c = (a && b) || ((a /= b) && c)

rotateLeft :: [a] -> Int -> [a]
rotateLeft list shiftNumber
    | shiftNumber < 0 = error "Shift number must be positive."
    | otherwise = take (length list) $ drop shiftNumber $ cycle list