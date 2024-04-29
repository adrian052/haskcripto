module Polygraphic.Playfair (
    encryptPlayfair,
    decryptPlayfair
) where


import Data.Char
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Maybe

--Exposed functions
encryptPlayfair :: String -> String -> String
encryptPlayfair text keyword = concatMap ((\(c1,c2) -> c1 : [c2]) . (\x -> encryptDigram x indexes matrix)) digrams
                            where matrix = createMatrix keyword
                                  indexes = alphIndexes matrix
                                  digrams = getDigrams text

decryptPlayfair :: String -> String -> String
decryptPlayfair text keyword = concatMap ((\(c1,c2) -> c1 : [c2]) . (\x -> decryptDigram x indexes matrix)) digrams
                            where matrix = createMatrix keyword
                                  indexes = alphIndexes matrix
                                  digrams = getDigrams text

--Algorithm functions
createList :: String -> String
createList keyword = keyword ++ abcWithoutKeys (Set.fromList keyword)
                            where
                            abcWithoutKeys keys = filter (\x -> x/='J' && Set.notMember x keys) ['A'..'Z']

createMatrix :: String -> [String]
createMatrix unformatedKey = toMatrix(createList $ formatKeyword unformatedKey)
                              where
                              chunk _ [] = []
                              chunk n xs = take n xs : chunk n (drop n xs)
                              toMatrix = chunk 5

alphIndexes :: [String] -> M.Map Char (Int, Int)
alphIndexes matrix = M.fromList $ map (\(row, col, char) -> (char, (row, col))) charCoords
    where charCoords = filter (\(_, _, c) -> c /= ' ') $ indexMatrix $ map (filter (/= ' ')) matrix


getDigrams :: String -> [(Char,Char)]
getDigrams text = buildDigrams $ formatText text

buildDigrams :: String -> [(Char,Char)]
buildDigrams []        = []
buildDigrams [x]       = [(x,'X')]
buildDigrams (x:y:z)
                    | x == y    = [(x,'X')] ++ buildDigrams (y:z)
                    | otherwise = [(x,y)]  ++ buildDigrams z


encryptDigram :: (Char,Char) -> M.Map Char (Int,Int)-> [String] -> (Char,Char)
encryptDigram (c1,c2) charIndex matrix
                    | row1 == row2      = (matrix!!(row1)!!(mod (col1+1) 5),matrix!!(row2)!!(mod (col2+1) 5))
                    | col1 == col2      = (matrix!!(mod (row1+1) 5)!!(col1),matrix!!(mod (row2+1) 5)!!(col2))
                    | otherwise         = (matrix!!row1!!col2,matrix!!row2!!col1)
                    where (row1, col1) = fromMaybe ((-1),(-1)) (M.lookup c1 charIndex)
                          (row2, col2) = fromMaybe ((-1),(-1)) (M.lookup c2 charIndex)

decryptDigram :: (Char, Char) -> M.Map Char (Int, Int) -> [String] -> (Char, Char)
decryptDigram (c1, c2) charIndex matrix
    | row1 == row2      = (getChar (row1, col1 - 1), getChar (row2, col2 - 1))
    | col1 == col2      = (getChar (row1 - 1, col1), getChar (row2 - 1, col2))
    | otherwise         = (matrix !! row1 !! col2, matrix !! row2 !! col1)
    where (row1, col1) = fromMaybe ((-1), (-1)) (M.lookup c1 charIndex)
          (row2, col2) = fromMaybe ((-1), (-1)) (M.lookup c2 charIndex)
          getChar (r, c)
            | r == -1   = matrix!!4!!c
            | c == -1   = matrix!!r!!4
            | otherwise = matrix!!r!!c

---Helper functions
convertJI :: String -> String
convertJI str = map (\x -> if x == 'J' then 'I' else x) str

toUpperWord :: String -> String
toUpperWord str = map toUpper str

formatKeyword :: String -> String
formatKeyword str = if ((all isAlpha str) && (all isAlpha str))
                    then toUpperWord $ removeDuplicated $ (convertJI str)
                    else error "Please insert alpha keyword"
                    where
                        removeDuplicated [] = []
                        removeDuplicated (x:xs) = x : removeDuplicated (filter (/= x) xs)

formatText :: String -> String
formatText str = if ((all isAlpha str) && (all isAlpha str))
                    then toUpperWord $ (convertJI str)
                    else error "Please insert alpha keyword"

indexMatrix :: [[a]] -> [(Int, Int, a)]
indexMatrix matrix = concatMap (\(rowIndex, row) -> zipWith (\colIndex elem -> (rowIndex, colIndex, elem)) [0..] row) indexedRows
    where indexedRows = zip [0..] matrix

coordToMap :: [(Int, Int, a)] -> M.Map (Int, Int) a
coordToMap = M.fromList . map (\(row, col, elem) -> ((row, col), elem))