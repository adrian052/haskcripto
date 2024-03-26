module Substitution.RailFence (
    encryptRailFence,
) where

import Data.List (sortBy)

labelRail :: String -> Bool -> Int -> Int -> Int -> [(Char,(Int, Int))]
labelRail [] _ _ _ _                 = []
labelRail (x:xs) down row col depth  = [(x,(row, col))] ++ labelRail xs newDown newRow (col+1) depth
                                    where newDown = if (down && depth==row) || (not(down) && row==1)
                                                    then not(down)
                                                    else down 
                                          newRow = if(newDown) then row+1 else row-1 

compareByFence :: (Char, (Int,Int)) -> (Char, (Int,Int)) -> Ordering
compareByFence (_, (a, b)) (_, (c, d))
    | a /= c   = compare a c
    | otherwise = compare b d

getChars :: [(Char, (Int, Int))] -> [Char]
getChars tuples = map (\(char, _) -> char) tuples

encryptRailFence :: String -> Int -> String
encryptRailFence str depth = getChars $ sortBy compareByFence (labelRail str True 1 1 depth)


labelRail2 :: String -> Bool -> Int -> Int -> Int -> [(Int,Int)]
labelRail2 [] _ _ _ _               = []
labelRail2 (_:xs) down row col depth  = [(row, col)] ++ labelRail2 xs newDown newRow (col+1) depth
                                    where newDown = if (down && depth==row) || (not(down) && row==1)
                                                    then not(down)
                                                    else down 
                                          newRow = if(newDown) then row+1 else row-1 

compareByFirst :: (Int,Int) -> (Int,Int) -> Ordering
compareByFirst (a, b) (c, d)
    | a /= c   = compare a c
    | otherwise = compare b d

compareByFence2 :: (Char, (Int,Int)) -> (Char, (Int,Int)) -> Ordering
compareByFence2 (_, (a, b)) (_, (c, d))
    | b /= d   = compare b d
    | otherwise = compare a c

decryptRailFence :: String -> Int -> String 
decryptRailFence str depth = 
    getChars (sortBy compareByFence2 (zip str (sortBy compareByFirst (labelRail2 str True 1 1 depth))))