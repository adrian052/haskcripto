module Block.Matrix where

import Data.List (transpose)

type Matrix a = [[a]]

rotateMatrixRight :: Matrix a -> Matrix a
rotateMatrixRight = transpose . reverse

rotateMatrixLeft :: Matrix a -> Matrix a
rotateMatrixLeft = reverse . transpose

permuteColumns :: Matrix a -> [Int] -> Matrix a
permuteColumns matrix indices = map (\row -> map (row !!) indices) matrix