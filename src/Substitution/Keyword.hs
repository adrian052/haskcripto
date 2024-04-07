module Substitution.Keyword (
    encryptKeyword,
    decryptKeyword
) where

import Data.Maybe(fromJust)
import Data.List (elemIndex)
import Data.Char ( isLetter, ord, toUpper )

prepareKeyword :: String -> String
prepareKeyword keyword = removeDuplicates (map toUpper keyword)

removeDuplicates :: String -> String
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

createEncryptionAlphabet :: String -> String
createEncryptionAlphabet keyword = prepareKeyword keyword ++ filter (`notElem` prepareKeyword keyword) ['A'..'Z']

encryptKeyword :: String -> String -> String
encryptKeyword keyword plaintext =
    let alphabet = createEncryptionAlphabet keyword
        encryptChar ch = if isLetter ch then alphabet !! (ord (toUpper ch) - ord 'A') else ch
    in map encryptChar plaintext

decryptKeyword :: String -> String -> String
decryptKeyword keyword ciphertext =
    let alphabet = createEncryptionAlphabet keyword
        decryptChar ch = if isLetter ch then ['A'..'Z'] !! fromJust (elemIndex ch alphabet) else ch
    in map decryptChar ciphertext