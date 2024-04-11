import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List
import Data.Word

type BitMatrix = [[Bool]]

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

--Initial permutation
initialPermutation :: BitMatrix -> BitMatrix
initialPermutation bitmatrix = rotateMatrix $ permuteColumns bitmatrix permutation

permutation :: [Int]
permutation = [1, 3, 5, 7, 0, 2, 4, 6]

permuteColumns :: BitMatrix -> [Int] -> BitMatrix
permuteColumns matrix indices = map (\row -> map (row !!) indices) matrix

rotateMatrix :: [[a]] -> [[a]]
rotateMatrix = transpose . reverse