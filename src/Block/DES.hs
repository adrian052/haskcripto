import qualified Data.ByteString.Char8 as BS

input :: BS.ByteString
input = BS.pack "Hola mi nombre es Adrian Ibarra Gonzalez"

blocksOf :: BS.ByteString -> Int -> [BS.ByteString]
blocksOf bytes length
  | BS.length bytes == 0 = []
  | BS.length bytes < length = [BS.append bytes (BS.replicate (length - BS.length bytes) 'x')]
  | otherwise = BS.take 8 bytes : blocksOf (BS.drop length bytes) length
