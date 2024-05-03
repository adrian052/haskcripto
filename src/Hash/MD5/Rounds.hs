import Data.Bits ( Bits(xor) )

round1 :: [Bool] -> [Bool] -> [Bool] -> [Bool]
round1 = zipWith3 round1Step
    where
        round1Step bi ci di = (bi && ci) || (not bi && di)

round2 :: [Bool] -> [Bool] -> [Bool] -> [Bool]
round2 = zipWith3 round2Step
    where
        round2Step bi ci di = (bi && di) || (ci && not di)

round3 :: [Bool] -> [Bool] -> [Bool] -> [Bool]
round3 = zipWith3 round3Step
    where
        round3Step bi ci di = bi `xor` ci `xor` di

round4 :: [Bool] -> [Bool] -> [Bool] -> [Bool]
round4 = zipWith3 round4Step
    where
        round4Step bi ci di = ci `xor` (bi || not di)
