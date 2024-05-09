module Hash.MD5.Round where

import Matrix
import Hash.MD5.Padding
import Hash.MD5.InitValues
import Data.Char (ord)
import Data.Bits (Bits (testBit))

blocks :: String -> [[Bool]]
blocks str = getBlocks (paddingMD5 $ stringToBits str)
    where 
    stringToBits :: String -> [Bool]
    stringToBits = concatMap (\c -> reverse [testBit (ord c) i | i <- [0 .. 7]])

words :: [Bool] -> [[Bool]]
words = group 16

buffers :: [[Bool]]
buffers = initializeBuffers