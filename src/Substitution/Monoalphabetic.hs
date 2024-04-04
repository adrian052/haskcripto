module Substitution.RailFence (
    encryptMonoalphabetic,
    decryptMonoalphabetic
) where

import qualified Data.Map as Map
import Data.Char

englishAlphabetPermutation :: [Char]
englishAlphabetPermutation = "ZYXWVUTSRQPONMLKJIHGFEDCBA"

mapEncrypt :: Map.Map Char Char
mapEncrypt = Map.fromList $ zip ['A'..'Z'] englishAlphabetPermutation

mapDecrypt :: Map.Map Char Char
mapDecrypt = Map.fromList $ zip englishAlphabetPermutation ['A'..'Z'] 

encryptMonoalphabetic :: String -> String
encryptMonoalphabetic message = map (\c -> Map.findWithDefault c c mapEncrypt) (formatText message)

decryptMonoalphabetic :: String -> String
decryptMonoalphabetic message = map (\c -> Map.findWithDefault c c mapDecrypt) (formatText message)


formatText :: String -> String
formatText str = if ((all isAlpha str) && (all isAlpha str))
                    then toUpperWord str
                    else error "Please insert alpha keyword"
                    where toUpperWord str = map toUpper str