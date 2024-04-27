module Block.DES.Round where

import Block.DES.Mangler (mangler)
import Block.Matrix (group, xorBitMatrix)

roundDES :: [Bool] -> [Bool] -> [Bool]
roundDES input key = result
  where
    (leftHalf, rightHalf) = splitAt 32 input
    rightHalfMatrix = group 4 rightHalf
    leftHalfMatrix = group 8 leftHalf
    keyMatrix = group 6 key
    mangleResult = mangler rightHalfMatrix keyMatrix
    leftHalfResult = xorBitMatrix leftHalfMatrix mangleResult
    result = rightHalf ++ concat leftHalfResult
