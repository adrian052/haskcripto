module Block.DES.Mangler where

import Block.DES.DES (bitMatrix)
import Block.Matrix (BitMatrix, addFirstColumn, addLastColumn, getColumn, xorBitMatrix)
import Data.Bits (Bits (xor))

expansionPermutation :: BitMatrix -> BitMatrix
expansionPermutation bitMatrix =
  if checkLenght bitMatrix
    then addLastColumn lastColumn $ addFirstColumn firstColumn bitMatrix
    else error "Please insert a matrix of 8x4"
  where
    firstColumn = bitMatrix !! 7 !! 3 : init (getColumn 3 bitMatrix)
    lastColumn = tail (getColumn 0 bitMatrix) ++ [head $ head bitMatrix]

xor48bits :: BitMatrix -> BitMatrix -> BitMatrix
xor48bits bitMatrix1 bitMatrix2 =
  if checkLenght bitMatrix1 && checkLenght bitMatrix2
    then xorBitMatrix bitMatrix1 bitMatrix2
    else error "Pleas insert a matrix of 8x4"

checkLenght :: Foldable t => [t a] -> Bool
checkLenght bits = length bits == 8 && length (head bits) == 4