module Block.DES.DES (encryptDESWord, decryptDESWord) where

import Block.DES.InitialPermutation (initialPermutation, inversePermutation)
import Block.DES.KeyScheduling (getKeys)
import Block.DES.Round (performRounds)
import Control.Arrow (ArrowChoice (right))
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.List
import Data.Word
import Matrix (BitMatrix, Matrix, group)

--Tests

input :: BSC.ByteString
input = BSC.pack "Hola mi nombre es Adrian Ibarra Gonzalez"

bitMatrix :: BitMatrix
bitMatrix = blockMatrix $ head (blocksOf input 8)

--Convert to blocks
blocksOf :: BS.ByteString -> Int -> [BS.ByteString]
blocksOf bytes length
  | BS.length bytes == 0 = []
  | BS.length bytes < length = [BS.append bytes (BS.replicate (length - BS.length bytes) $ fromIntegral (fromEnum 'x'))]
  | otherwise = BS.take 8 bytes : blocksOf (BS.drop length bytes) length

--Convert to bit matrix
blockMatrix :: BS.ByteString -> BitMatrix
blockMatrix bytes
  | BS.length bytes == 0 = []
  | otherwise = word8ToBitsList (BS.head bytes) : blockMatrix (BS.tail bytes)
  where
    word8ToBits :: Word8 -> [Bool]
    word8ToBits w = reverse [testBit w i | i <- [0 .. 7]]
    word8ToBitsList :: Word8 -> [Bool]
    word8ToBitsList = word8ToBits

padToMultipleOf8 :: BS.ByteString -> BS.ByteString
padToMultipleOf8 bs =
  let len = BS.length bs
      paddingNeeded = if len `mod` 8 == 0 then 0 else 8 - (len `mod` 8)
      padding = BS.replicate paddingNeeded 32
   in bs `BS.append` padding

bitsToWord8 :: [Bool] -> Word8
bitsToWord8 bools = foldl (\acc (i, b) -> if b then setBit acc i else clearBit acc i) 0 indexedBools
  where
    indexedBools = zip [0 .. 7] (reverse bools)

word8ListToAsciiString :: [Word8] -> String
word8ListToAsciiString = map (chr . fromIntegral)

encryptBlockDES :: BitMatrix -> [Bool] -> BitMatrix
encryptBlockDES block key = result
  where
    keys = getKeys key
    initPermutation = initialPermutation block
    flatInitPermutation = concat initPermutation
    roundsResult = performRounds flatInitPermutation keys
    (left, right) = splitAt 32 roundsResult
    swap32 = right ++ left
    matrixPreInverse = Block.Matrix.group 8 swap32
    result = inversePermutation matrixPreInverse

decryptBlockDES :: BitMatrix -> [Bool] -> BitMatrix
decryptBlockDES block key = result
  where
    keys = reverse (getKeys key)
    initPermutation = initialPermutation block
    flatInitPermutation = concat initPermutation
    roundsResult = performRounds flatInitPermutation keys
    (left, right) = splitAt 32 roundsResult
    swap32 = right ++ left
    matrixPreInverse = Block.Matrix.group 8 swap32
    result = inversePermutation matrixPreInverse

--Functions to export

encryptDESWord :: BS.ByteString -> BS.ByteString -> String
encryptDESWord word key
  | BS.length key == 8 = word8ListToAsciiString (map bitsToWord8 flatOneLevel)
  | otherwise = error "Please insert a 8 char key"
  where
    flatOneLevel = concat encryptedBlocks
    encryptedBlocks = map (`encryptBlockDES` keyBitList) blocks
    blocks = map blockMatrix (blocksOf (padToMultipleOf8 word) 8)
    keyBitList = concat (blockMatrix key)

decryptDESWord :: BS.ByteString -> BS.ByteString -> String
decryptDESWord word key
  | BS.length key == 8 = word8ListToAsciiString (map bitsToWord8 flatOneLevel)
  | otherwise = error "Please insert a 8 char key"
  where
    flatOneLevel = concat encryptedBlocks
    encryptedBlocks = map (`decryptBlockDES` keyBitList) blocks
    blocks = map blockMatrix (blocksOf (padToMultipleOf8 word) 8)
    keyBitList = concat (blockMatrix key)