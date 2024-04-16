module Block.DES.Mangler where

import Block.Matrix (BitMatrix, addFirstColumn, addLastColumn, getColumn)

expansionPermutation :: BitMatrix -> BitMatrix
expansionPermutation bitMatrix =
  if checkLenght bitMatrix
    then addLastColumn lastColumn $ addFirstColumn firstColumn bitMatrix
    else error "Please insert a matrix of 8x4"
  where
    checkLenght bits = length bits == 8 && length (head bits) == 4
    firstColumn = bitMatrix !! 7 !! 3 : init (getColumn 3 bitMatrix)
    lastColumn = tail (getColumn 0 bitMatrix) ++ [head $ head bitMatrix]