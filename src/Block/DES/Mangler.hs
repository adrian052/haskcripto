module Block.DES.Mangler where

import Block.DES.DES (bitMatrix)
import Block.Matrix (BitMatrix, Matrix, addFirstColumn, addLastColumn, getColumn, group, xorBitMatrix)
import Control.Monad (join)
import Data.Bits (Bits (xor))
import Data.List (unfoldr)
import Data.Sequence (chunksOf)

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

sixByRow :: BitMatrix -> BitMatrix
sixByRow bitMatrix = group 6 (concat bitMatrix)

reduceBoxS1 :: [Bool] -> [Bool]
reduceBoxS1 bool = leftPad False 4 (intToBoolList (boxS1 !! row !! col))
  where
    row = boolListToInt (head bool : [last bool])
    col = boolListToInt (slice 1 4 bool)
    slice from to xs = take (to - from + 1) (drop from xs)
    boolListToInt bools = sum [if bit then 2 ^ index else 0 | (bit, index) <- zip (reverse bools) [0 ..]]
    intToBoolList 0 = [False]
    intToBoolList n = reverse $ unfoldr (\x -> if x == 0 then Nothing else Just (odd x, div x 2)) n
    leftPad elemToAdd n list = replicate (n - length list) elemToAdd ++ list

--Helper functions and ds
checkLenght :: Foldable t => [t a] -> Bool
checkLenght bits = length bits == 8 && length (head bits) == 4

boxS1 :: Matrix Int
boxS1 =
  [ [14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7],
    [0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8],
    [4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0],
    [15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13]
  ]