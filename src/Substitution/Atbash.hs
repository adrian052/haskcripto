module Substitution.Atbash (
    encryptAtbash
) where

import Data.Char

encryptAtbash :: String -> String
encryptAtbash = map substitute
    where
        substitute :: Char -> Char
        substitute char
            | isLower char = substituteChar 'a' 'z' char
            | isUpper char = substituteChar 'A' 'Z' char
            | otherwise = char
        substituteChar :: Char -> Char -> Char -> Char
        substituteChar a z char = chr $ (ord z) - (ord char - ord a)