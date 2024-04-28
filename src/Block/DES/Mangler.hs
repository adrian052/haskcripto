module Block.DES.Mangler where

import Block.Matrix (BitMatrix, Matrix, addFirstColumn, addLastColumn, applyPermutation, getColumn, group, xorBitMatrix)
import Control.Monad (join)
import Data.Bits (Bits (complement, shiftL, shiftR, xor, (.&.), (.|.)))
import Data.List (elemIndex, sortOn, unfoldr)
import Data.Maybe (fromJust)
import Data.Sequence ()

mangler :: BitMatrix -> BitMatrix -> BitMatrix
mangler bitMatrix key = finalMatrix
  where
    expandedMatrix = expansionPermutation bitMatrix
    xorResult = xor48bits expandedMatrix key
    sixByRowResult = sixByRow xorResult
    s1Result = performBoxS1 sixByRowResult
    s1Result8Row = group 8 (concat s1Result)
    finalMatrix = applyPermutation lastPermutationMatrix s1Result8Row

--Algorithm functions
expansionPermutation :: BitMatrix -> BitMatrix
expansionPermutation bitMatrix =
  if checkLenght bitMatrix
    then addLastColumn lastColumn $ addFirstColumn firstColumn bitMatrix
    else error "Please insert a matrix of 4x8"
  where
    firstColumn = bitMatrix !! 7 !! 3 : init (getColumn 3 bitMatrix)
    lastColumn = tail (getColumn 0 bitMatrix) ++ [head $ head bitMatrix]

xor48bits :: BitMatrix -> BitMatrix -> BitMatrix
xor48bits = xorBitMatrix

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

performBoxS1 :: BitMatrix -> BitMatrix
performBoxS1 = map reduceBoxS1

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

lastPermutationMatrix :: [[(Int, Int)]]
lastPermutationMatrix =
  [ [(1, 7), (0, 6), (2, 3), (2, 4), (3, 4), (1, 3), (3, 3), (2, 0)],
    [(0, 0), (1, 6), (2, 6), (3, 1), (0, 4), (2, 1), (3, 6), (1, 1)],
    [(0, 1), (0, 7), (2, 7), (1, 5), (3, 7), (3, 2), (0, 2), (1, 0)],
    [(2, 2), (1, 4), (3, 5), (0, 5), (2, 5), (1, 2), (0, 3), (3, 0)]
  ]
