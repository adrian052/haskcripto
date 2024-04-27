module Block.DES.KeyScheduling where

import Block.Matrix (BitMatrix)
import qualified Data.Set as Set

getKeys :: [a] -> [[a]]
getKeys list =
  if length list == 64
    then performScheduling (permutatedChoice1 list) 1
    else error "Please insert a list of 64 elems"

performScheduling :: [a] -> Int -> [[a]]
performScheduling lastList round
  | round == 17 = []
  | otherwise = result : performScheduling ls (round + 1)
  where
    ls = if round `elem` [1, 2, 9, 16] then leftShift 1 lastList else leftShift 2 lastList
    result = permutatedChoice2 ls

permutatedChoice1 :: [a] -> [a]
permutatedChoice1 = dropByIndex (Set.fromList [8, 16, 24, 32, 40, 48, 56, 64]) 1

permutatedChoice2 :: [a] -> [a]
permutatedChoice2 = dropByIndex (Set.fromList [9, 18, 22, 25, 35, 38, 43, 54]) 1

leftShift :: Int -> [a] -> [a]
leftShift positions vector =
  let len = length vector
      halfLen = len `div` 2
      (firstHalf, secondHalf) = splitAt halfLen vector
      shiftedFirstHalf = drop positions firstHalf ++ take positions firstHalf
      shiftedSecondHalf = drop positions secondHalf ++ take positions secondHalf
   in shiftedFirstHalf ++ shiftedSecondHalf

dropByIndex :: Set.Set Int -> Int -> [a] -> [a]
dropByIndex _ _ [] = []
dropByIndex dropList i (x : xs) =
  if Set.member i dropList
    then dropByIndex dropList (i + 1) xs
    else x : dropByIndex dropList (i + 1) xs