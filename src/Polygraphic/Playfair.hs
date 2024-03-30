
import Data.Char
import qualified Data.Set as Set

formatKeyword :: String -> String
formatKeyword str = if ((all isAlpha str) && (all isAlpha str))
                    then toUpperWord $ removeDuplicated $ (convertJI str)
                    else error "Please insert alpha keyword"
                    where
                        removeDuplicated [] = []
                        removeDuplicated (x:xs) = x : removeDuplicated (filter (/= x) xs)
                        convertJI str = map (\x -> if x == 'J' then 'I' else x) str
                        toUpperWord str = map toUpper str

createDiagramList :: String -> String
createDiagramList keyword = keyword ++ abcWithoutKeys (Set.fromList keyword) 
                            where
                            abcWithoutKeys keys = filter (\x -> x/='J' && Set.notMember x keys) ['A'..'Z']

createDiagram :: String -> [String] 
createDiagram unformatedKey = toMatrix(createDiagramList $ formatKeyword unformatedKey)
                              where
                              chunk _ [] = []
                              chunk n xs = take n xs : chunk n (drop n xs)
                              toMatrix = chunk 5