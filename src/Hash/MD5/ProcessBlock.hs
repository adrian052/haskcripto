module Hash.MD5.ProcessBlock where

import Prelude hiding (round)
import Hash.MD5.Round
import Hash.MD5.Functions
import Hash.MD5.InitValues
import Matrix


processBlock :: [[Bool]] -> [Bool] -> [[Bool]] 
processBlock buffer block = zipWith (sumMod32) buffer resultR4
    where
    m1 = group 16 block
    m2 = permutate m1 [1,6,11,0,5,10,15,4,9,14,3,8,13,2,7,12]
    m3 = permutate m1 [5,8,11,14,1,4,7,10,13,0,3,6,9,12,15,2]
    m4 = permutate m1 [0,7,14,5,12,3,10,1,8,15,6,13,4,11,2,9]
    resultR1 = round f buffer m1 k1 (s!!0) 0
    resultR2 = round g resultR1 m2 k2 (s!!1) 0
    resultR3 = round h resultR2 m3 k3 (s!!2) 0
    resultR4 = round i resultR3 m4 k4 (s!!3) 0
    

boolSum :: [Bool] -> [Bool] -> [Bool]
boolSum [] [] = []
boolSum (x:xs) (y:ys) = (x || y) : boolSum xs ys

permutate :: [a] -> [Int] -> [a]
permutate xs p = map (\i -> xs !! i) p


s :: [[Int]]
s = [ [7,12,17,22,7,12,17,22,7,12,17,22,7,12,17,22]
    , [5,9,14,20,5,9,14,20,5,9,14,20,5,9,14,20]
    , [4,11,16,23,4,11,16,23,4,11,16,23,4,11,16,23]
    , [6,10,15,21,6,10,15,21,6,10,15,21,6,10,15,21]
    ]