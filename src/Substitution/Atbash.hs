module Substitution.Atbash (
    encryptAtbash
) where

import Data.Char

encryptAtbash :: String -> Either String String
encryptAtbash = mapM substitute
    where
        substitute :: Char -> Either String Char
        substitute char
            | isLower char = Right $ substituteChar 'a' 'z' char
            | isUpper char = Right $ substituteChar 'A' 'Z' char
            | otherwise = Left "Please insert alphabet keys"
        substituteChar :: Char -> Char -> Char -> Char
        substituteChar a z char = chr $ ord z - (ord char - ord a)