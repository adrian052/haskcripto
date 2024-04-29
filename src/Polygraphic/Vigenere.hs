module Polygraphic.Vigenere (
    encryptVigenere,
    decryptVigenere
) where


import Data.Char

encryptChar :: (Char, Char) -> Char
encryptChar (char, key) = shift numKey char
                        where numKey = ord key - ord 'A'

decryptChar :: (Char, Char) -> Char
decryptChar (char, key) = shift numKey char
                        where numKey = -(ord key - ord 'A')

shift :: Int -> Char -> Char
shift shift char = chr $ (ord 'A' + (ord char - ord 'A' + shift) `mod` 26)

toUpperWord :: String -> String
toUpperWord = map toUpper

repeatAndConcat :: String -> String
repeatAndConcat str = str ++ repeatAndConcat str

encryptVigenere :: String -> String -> String
encryptVigenere word keyword = if ((all isAlpha word) && (all isAlpha keyword))
                                then map encryptChar zipped  
                                else error "Cannot encrypt, please use alpha chars"
                                where upperWord = toUpperWord word
                                      upperKeyword = toUpperWord keyword
                                      zipped = zip upperWord (repeatAndConcat upperKeyword)


decryptVigenere :: String -> String -> String
decryptVigenere word keyword = if ((all isAlpha word) && (all isAlpha keyword))
                                then map decryptChar zipped  
                                else error "Cannot encrypt use alpha chars"
                                where upperWord = toUpperWord word
                                      upperKeyword = toUpperWord keyword
                                      zipped = zip upperWord (repeatAndConcat upperKeyword)