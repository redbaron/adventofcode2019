module Day1 where

import           Data.Maybe

fuelFromMassMaybe :: Int -> Maybe Int
fuelFromMassMaybe m = if r > 0 then Just r else Nothing
    where r = m `div` 3 - 2

fuelFromMass :: Int -> Int
fuelFromMass m = fromMaybe 0 $ fuelFromMassMaybe m

seq' :: Num a => Maybe a -> (a -> Maybe a) -> a
seq' (Just x) f = fromMaybe 0 r + seq' r f where r = f x
seq' Nothing  _ = 0

-- accounts for mass of a fuel itself
fuelFromMassAndFuel :: Int -> Int
fuelFromMassAndFuel m = seq' (Just m) fuelFromMassMaybe
