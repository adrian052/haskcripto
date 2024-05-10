module Hash.MD5.MD5 where

import Data.Bits (Bits (testBit))
import Data.Char (ord)
import Hash.MD5.Functions (f)
import Hash.MD5.InitValues (initializeBuffers, kConstants)
import Hash.MD5.Padding (getBlocks, paddingMD5)
import Hash.MD5.ProcessBlock
import Data.Word (Word8)
import Numeric (showHex)

--Get blocks from input
blocks :: String -> [[Bool]]
blocks str = getBlocks (paddingMD5 $ stringToBits str)
    where 
    stringToBits :: String -> [Bool]
    stringToBits = concatMap (\c -> reverse [testBit (ord c) i | i <- [0 .. 7]])


md5 :: String -> String
md5 msg =  md5ToHex (concat processed)
    where
        processed = foldl (\buffer block -> processBlock buffer block) initialHash b
        b = blocks msg
        initialHash = initializeBuffers



-- Convierte una lista de booleanos en una lista de bytes
boolsToBytes :: [Bool] -> [Word8]
boolsToBytes [] = []
boolsToBytes bs = fromIntegral (boolsToInt (take 8 bs)) : boolsToBytes (drop 8 bs)
    where
        boolsToInt = foldl (\acc b -> if b then acc * 2 + 1 else acc * 2) 0

-- Convierte un byte a su representación hexadecimal como String
byteToHex :: Word8 -> String
byteToHex b = pad $ showHex b ""
    where pad str = replicate (2 - length str) '0' ++ str

-- Convierte un hash MD5 de lista de booleanos a una cadena hexadecimal
md5ToHex :: [Bool] -> String
md5ToHex md5hash = concatMap byteToHex $ boolsToBytes md5hash

