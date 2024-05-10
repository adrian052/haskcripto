module Hash.MD5.Functions where

import Data.Bits (Bits (xor))

f :: [Bool] -> [Bool] -> [Bool] -> [Bool]
f = zipWith3 (\bi ci di -> (bi && ci) || ((not bi) && di))

g :: [Bool] -> [Bool] -> [Bool] -> [Bool]
g = zipWith3 (\bi ci di -> (bi && di) || (ci && (not di)))

h :: [Bool] -> [Bool] -> [Bool] -> [Bool]
h = zipWith3 (\bi ci di -> bi `xor` ci `xor` di)

i :: [Bool] -> [Bool] -> [Bool] -> [Bool]
i = zipWith3 (\bi ci di -> ci `xor` (bi || (not di)))
