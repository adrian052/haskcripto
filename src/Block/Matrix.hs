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

group :: Int -> [a] -> Matrix a
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"

applyPermutation :: [[(Int, Int)]] -> Matrix a -> Matrix a
applyPermutation permutationMatrix matrix =
  let numRows = length matrix
      numCols = length (head matrix)
      newPosition (r, c) = (permutationMatrix !! r !! c)
      newElement (r, c) = matrix !! r !! c
      result = [[newElement (newPosition (r, c)) | c <- [0 .. numCols -1]] | r <- [0 .. numRows -1]]
   in result

xorBitMatrix :: BitMatrix -> BitMatrix -> BitMatrix
xorBitMatrix = zipWith $ zipWith xor
