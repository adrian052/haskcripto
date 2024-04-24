module Block.DES.KeyScheduling where

import Block.Matrix (BitMatrix)
import qualified Data.Set as Set

permutedChoice1 :: BitMatrix -> BitMatrix
permutedChoice1 bitmatrix =
  if checkLenght bitmatrix
    then map init bitmatrix
    else error "Please insert a 8x8 bitmatrix"
  where
    checkLenght bits = length bits == 8 && length (head bits) == 8

leftShift :: Int -> [a] -> [a]
leftShift positions vector = drop positions vector ++ take positions vector

dropByIndex :: Set.Set Int -> Int -> [a] -> [a]
dropByIndex _ _ [] = []
dropByIndex dropList i (x : xs) =
  if Set.member i dropList
    then dropByIndex dropList (i + 1) xs
    else x : dropByIndex dropList (i + 1) xs