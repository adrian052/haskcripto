import Data.Bits
import Data.List

paddingMD5 :: [Bool] -> [Bool]
paddingMD5 input = paddedInput
  where
    originalLength = length input
    paddedLength = (originalLength + 1 + paddingLength) `mod` 512
    paddingLength = (448 - (originalLength + 1) `mod` 512) `mod` 512
    padding = True : replicate paddingLength False
    paddedInput = input ++ padding ++ lengthToBits originalLength
    lengthToBits :: Int -> [Bool]
    lengthToBits len = reverse $ take 64 $ unfoldr (\x -> if x == 0 then Nothing else Just (testBit x 0, shiftR x 1)) len