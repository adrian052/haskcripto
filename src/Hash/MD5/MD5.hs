module Hash.MD5.MD5 where

import Data.Bits (Bits (testBit))
import Data.Char (ord)
import Hash.MD5.Functions (f)
import Hash.MD5.InitValues (initializeBuffers, kConstants)
import Hash.MD5.Padding (getBlocks, paddingMD5)

--Get M from input
m :: String -> [[Bool]]
m str = getBlocks (stringToBits str)
    where 
    stringToBits :: String -> [Bool]
    stringToBits = concatMap (\c -> reverse [testBit (ord c) i | i <- [0 .. 7]])
