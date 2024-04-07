module Substitution.Caesar (
    encryptCaesar,
    decryptCaesar
) where

import Data.Char ( ord, chr, isAsciiLower, isAsciiUpper )

type CaesarResult = Either String Char

shift :: Int -> Char -> CaesarResult
shift shift char
    | isAsciiUpper char = Right $ chr (ord 'A' + (ord char - ord 'A' + shift) `mod` 26)
    | isAsciiLower char = Right $ chr (ord 'a' + (ord char - ord 'a' + shift) `mod` 26)
    | otherwise = Left "Error: non alphabetic char"

encryptCaesar :: String -> Either String String
encryptCaesar = mapM (shift 3)

decryptCaesar :: String -> Either String String
decryptCaesar = mapM (shift (-3))