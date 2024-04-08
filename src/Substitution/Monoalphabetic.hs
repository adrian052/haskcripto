module Substitution.RailFence
  ( encryptMonoalphabetic,
    decryptMonoalphabetic,
  )
where

import Data.Char (isAlpha, toUpper)
import qualified Data.Map as Map

englishAlphabetPermutation :: [Char]
englishAlphabetPermutation = "ZYXWVUTSRQPONMLKJIHGFEDCBA"

mapEncrypt :: Map.Map Char Char
mapEncrypt = Map.fromList $ zip ['A' .. 'Z'] englishAlphabetPermutation

mapDecrypt :: Map.Map Char Char
mapDecrypt = Map.fromList $ zip englishAlphabetPermutation ['A' .. 'Z']

encryptMonoalphabetic :: String -> Maybe String
encryptMonoalphabetic message = mapM (`Map.lookup` mapEncrypt) (toUpperWord message)

decryptMonoalphabetic :: String -> Maybe String
decryptMonoalphabetic message = mapM (`Map.lookup` mapDecrypt) (toUpperWord message)

toUpperWord :: String -> String
toUpperWord = map toUpper