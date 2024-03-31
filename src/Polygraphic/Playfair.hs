
import Data.Char
import qualified Data.Set as Set

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

getDiagrams :: String -> [(Char,Char)]
getDiagrams text = buildDiagrams $ formatText text

buildDiagrams :: String -> [(Char,Char)]
buildDiagrams []        = []
buildDiagrams [x]       = [(x,'X')]
buildDiagrams (x:y:z)   
                    | x == y    = [(x,'X')] ++ buildDiagrams (y:z)
                    | otherwise = [(x,y)]  ++ buildDiagrams z


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