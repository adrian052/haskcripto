
import Data.Char

encryptAutokey :: String -> Int -> String
encryptAutokey text key = map (\sum -> chr ((mod sum 26)+ord 'A')) (zipWith (+) pvalues keystream') 
            where 
            pvalues =  pValues $ formatText text
            keystream' = keystream key pvalues

--HELPER FUNCTIONS 

keystream :: Int -> [Int] -> [Int]
keystream head pValues = head: pValues

pValues :: String -> [Int]
pValues = map (\x -> ord x - ord 'A')

formatText :: String -> String
formatText str = if ((all isAlpha str) && (all isAlpha str))
                    then toUpperWord str
                    else error "Please insert alpha keyword"
                    where 
                    toUpperWord str = map toUpper strformatText $