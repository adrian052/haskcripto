module Block.DES.KeyScheduling where

import Block.Matrix (BitMatrix)

permutedChoice1 :: BitMatrix -> BitMatrix
permutedChoice1 bitmatrix =
  if checkLenght bitmatrix
    then map init bitmatrix
    else error "Please insert a 8x8 bitmatrix"
  where
    checkLenght bits = length bits == 8 && length (head bits) == 8

leftShift :: Int -> [a] -> [a]
leftShift positions vector = drop positions vector ++ take positions vector