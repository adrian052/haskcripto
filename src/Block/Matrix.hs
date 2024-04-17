module Block.Matrix where

import Control.Monad
import Data.Bits (Bits (xor))
import Data.List (transpose)

type Matrix a = [[a]]

type BitMatrix = Matrix Bool

rotateMatrixRight :: Matrix a -> Matrix a
rotateMatrixRight = transpose . reverse

rotateMatrixLeft :: Matrix a -> Matrix a
rotateMatrixLeft = reverse . transpose

permuteColumns :: Matrix a -> [Int] -> Matrix a
permuteColumns matrix indices = map (\row -> map (row !!) indices) matrix

addFirstColumn :: [a] -> Matrix a -> Matrix a
addFirstColumn = zipWith (:)

addLastColumn :: [a] -> Matrix a -> Matrix a
addLastColumn col matrix = zipWith (\row c -> row ++ [c]) matrix col

getColumn :: Int -> Matrix a -> [a]
getColumn colIndex = map (!! colIndex)

xorBitMatrix :: BitMatrix -> BitMatrix -> BitMatrix
xorBitMatrix = zipWith $ zipWith xor

group :: Int -> [a] -> Matrix a
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"