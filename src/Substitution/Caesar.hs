module Substitution.Caesar (
    encryptCaesar,
    decryptCaesar
) where

import Data.Char

encryptCaesar :: String -> String
encryptCaesar = map $ shift 3

decryptCaesar :: String -> String
decryptCaesar = map $ shift (-3)

shift :: Int -> Char -> Char
shift shift char = chr $ (ord char + (shift)) `mod` 128