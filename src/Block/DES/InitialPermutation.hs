module Block.DES.InitialPermutation (initialPermutation, inversePermutation) where

import Block.Matrix
  ( Matrix,
    permuteColumns,
    rotateMatrixLeft,
    rotateMatrixRight,
  )

initialPermutation :: Matrix a -> Matrix a
initialPermutation bitmatrix = rotateMatrixRight $ permuteColumns bitmatrix permutation

inversePermutation :: Matrix a -> Matrix a
inversePermutation bitmatrix = permuteColumns (rotateMatrixLeft bitmatrix) permutation2

permutation :: [Int]
permutation = [1, 3, 5, 7, 0, 2, 4, 6]

permutation2 :: [Int]
permutation2 = [4, 0, 5, 1, 6, 2, 7, 3]
