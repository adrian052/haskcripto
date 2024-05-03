

initializeBuffers :: [[Bool]]
initializeBuffers = concatMap hexToBits32 initialValuesHex

initialValuesHex :: [String]
initialValuesHex = ["67452301", "EFCDAB89", "98BADCFE", "10325476"]

hexToBits :: Char -> [Bool]
hexToBits '0' = replicate 4 False
hexToBits '1' = [False, False, False, True]
hexToBits '2' = [False, False, True, False]
hexToBits '3' = [False, False, True, True]
hexToBits '4' = [False, True, False, False]
hexToBits '5' = [False, True, False, True]
hexToBits '6' = [False, True, True, False]
hexToBits '7' = [False, True, True, True]
hexToBits '8' = [True, False, False, False]
hexToBits '9' = [True, False, False, True]
hexToBits 'A' = [True, False, True, False]
hexToBits 'B' = [True, False, True, True]
hexToBits 'C' = [True, True, False, False]
hexToBits 'D' = [True, True, False, True]
hexToBits 'E' = [True, True, True, False]
hexToBits 'F' = [True, True, True, True]
hexToBits _   = error "Caracter hexadecimal not valid"

hexStringToBits :: String -> [Bool]
hexStringToBits = concatMap hexToBits

hexToBits32 :: String -> [[Bool]]
hexToBits32 hexString = chunksOf 4 $ hexStringToBits hexString

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)


