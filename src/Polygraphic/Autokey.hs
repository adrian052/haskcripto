module Substitution.RailFence (
    encryptAutokey,
    decryptAutokey
) where

import Data.Char

charToNum :: Char -> Int
charToNum c = ord c - ord 'A'

numToChar :: Int -> Char
numToChar n = chr (n `mod` 26 + ord 'A')

formatText :: String -> String
formatText str = if ((all isAlpha str) && (all isAlpha str))
                    then toUpperWord str
                    else error "Please insert alpha keyword"

toUpperWord :: String -> String
toUpperWord str = map toUpper str

encryptAutokey :: String -> String -> String
encryptAutokey key plaintext = map numToChar $ zipWith (\p k -> (charToNum p + charToNum k) `mod` 26) (formatText plaintext) (cycle key)

decryptAutokey :: String -> String -> String
decryptAutokey key ciphertext = map numToChar $ zipWith (\c k -> (charToNum c - charToNum k) `mod` 26) ciphertext (cycle key)
