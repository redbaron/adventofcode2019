module Day1 where

fuelFromMass :: Int -> Int
fuelFromMass m = m `div` 3 - 2

-- accounts for mass of a fuel itself
fuelFromMassAndFuel :: Int -> Int
fuelFromMassAndFuel m
    | m > (3 * 2) = let f = fuelFromMass m in f + fuelFromMassAndFuel f
    | otherwise   = 0
