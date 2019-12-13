module Day1 where

import           Data.Maybe
import           Control.Monad
import           Data.Monoid

-- part 1
fuel :: Int -> Maybe Int
fuel m = if r > 0 then Just r else Nothing where r = m `div` 3 - 2

fuelFromMass :: Int -> Int
fuelFromMass m = fromMaybe 0 $ fuel m

-- part 2

--- recursion
f' :: Num a => Maybe a -> (a -> Maybe a) -> a
f' (Just x) fn = fromMaybe 0 r + f' r fn where r = fn x
f' Nothing  _  = 0

fuelFromMassAndFuel :: Int -> Int
fuelFromMassAndFuel m = f (Just m) fuel

--- monad
f :: Maybe Int -> (Int -> Maybe Int) -> Int
f x fn = fromMaybe 0 $ do
    v <- x
    let n    = fn v
    let next = f n fn
    x' <- n
    return (x' + next)

fuelFromMassAndFuel' :: Int -> Int
fuelFromMassAndFuel' m = f (Just m) fuel
