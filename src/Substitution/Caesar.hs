module Substitution.Caesar (
    encryptCaesar,
    decryptCaesar
) where

import Data.Char ( ord, chr, isAsciiLower, isAsciiUpper )

data CaesarError = NonAlphabeticChar | InvalidShift deriving (Show)

type CaesarResult = Either CaesarError Char

shift :: Int -> Char -> CaesarResult
shift shift char
    | isAsciiUpper char = Right $ chr (ord 'A' + (ord char - ord 'A' + shift) `mod` 26)
    | isAsciiLower char = Right $ chr (ord 'a' + (ord char - ord 'a' + shift) `mod` 26)
    | otherwise = Left NonAlphabeticChar

encryptCaesar :: String -> Either CaesarError String
encryptCaesar = mapM (shift 3)

decryptCaesar :: String -> Either CaesarError String
decryptCaesar = mapM (shift (-3))