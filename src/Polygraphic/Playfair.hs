
import Data.Char

formatKeyword :: String -> String
formatKeyword str = if ((all isAlpha str) && (all isAlpha str))
                    then toUpperWord $ removeDuplicated $ (convertJI str)
                    else error "Please insert alpha keyword"
                    where
                        removeDuplicated [] = []
                        removeDuplicated (x:xs) = x : removeDuplicated (filter (/= x) xs)
                        convertJI str = map (\x -> if x == 'J' then 'I' else x) str
                        toUpperWord str = map toUpper str