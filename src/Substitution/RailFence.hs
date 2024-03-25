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